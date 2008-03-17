#include <string.h>
#include <tk.h>

/* This file contains an implementation of a Tk image type that shows
   the contents of an R graphics device.  You create an image as

   	image create Rplot ?name?

   The current device is used, and the current device is closed when
   the image is created (unless there is an error, in which case the
   device may remain open).  For X11 the device should be XImage (a
   new variant of X11).  For Windows, the device must be win.metafile
   with no file argument, which means the result is sent to the
   clipboard.  The metafile is retrieved from the clipboard and placed
   in the Rplot image; this means that the clibboard is always
   modified when an Rplot image is created on windows (a gross hack,
   but it allows the windows device code to be used with no
   modification).

   Here is a very simple example suitable for pasting into R:

        if (Sys.info()["sysname"] == "Windows") {
            my.tkdev <- function() win.metafile(width=4,height=4)
        } else {
            my.tkdev <- function()
	        .Internal(X11("XImage", 480, 480, 12, 1,
                              getOption("X11colortype"), 256))
        }

        my.tkdev()
        plot(1:20)
        .Tcl("image create Rplot fred")
        base<-tktoplevel()
        lab<-tklabel(base,image="fred")

        bb<-1
        f<-function(...) {
            b <- as.numeric(tclvar$bb)
            if (b != bb) {
                bb <<- b
                my.tkdev()
                plot(1:20,(1:20)^b)
                .Tcl("image create Rplot fred")
            }
        }

        s <- tkscale(base, command=f, from=0.05, to=2.00, variable="bb",
                     showvalue=F, resolution=0.05, orient="horiz")
        tkpack(lab, s)

   Performance under X11 is less than ideal since it relies on copying
   an image from the server to the client and then back again.  But
   there is no visible flicker, at least on Linux.

   On windows the performance is also not ideal and there is
   occasional flicker, which could probably be avoided by playing the
   metafile to a pixmap and then blasting in the pixmap with a single
   BitBlt, but I haven't tried that yet.

   This is a temporary hack at best--it would be better to use a
   mechanism in which the content is drawn from the display list, and
   this could be done by abstracting out the drawing code in the
   driver so it can be used in any drawable.

   This implementation is based on the Tk image extention library
   at http://purl.oclc.org/net/nijtmans/img.html. -- LT */

#include "Rinternals.h"
#include "Rversion.h"
#if R_VERSION < R_Version(2,7,0)
# include "Rgraphics.h"
# include "Rdevices.h"  /* for killDevice */
#else
# include <R_ext/GraphicsEngine.h>
#endif

/*
 * Image data structures 
 */
#ifdef Win32
#include <windows.h>

/* This is taken from the Tk source headers.  This is a REALLY BAD
   idea, but the only alternative is to keep the Tk sources around in
   order to compile this. This declaration is needed for
   TkWinGetDrawableDC and TkWinReleaseDrawableDC. */
typedef struct TkWinDCState {
    HPALETTE palette;
} TkWinDCState;

/* These declarations don't even appear in the Tk source heaters */
HDC TkWinGetDrawableDC();
void TkWinReleaseDrawableDC();

typedef HENHMETAFILE RplotImage;
#else
#include <X11/Xutil.h>
typedef XImage *RplotImage;
#include <R_ext/GetX11Image.h>
#endif 

typedef struct RplotMaster {
    Tcl_Interp *interp;		/* interpreter that owns the image */
    int Rdevice;		/* R device number */
    int haveImage;		/* boolean: is image installed? */
    RplotImage image;		/* image of R device */
    int width, height;		/* image dimensions */
    int instanceCount;		/* number of instances */
    Tk_ImageMaster master;	/* Tk's token for image master */
} RplotMaster;

typedef struct RplotInstance {
    RplotMaster *master;	/* pointer to master for image */
    Tk_Window tkwin;		/* window for the instance */
} RplotInstance;


/*
 *----------------------------------------------------------------------
 *
 * GetRplotImage --
 *
 *	This procedure gets the image and the size information for a
 *	specified R device.  Some checking is done to make sure the
 *	device is valid.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	The device is closed.  On Windows the clipboard is modified.
 *
 *---------------------------------------------------------------------- */
static int
GetRplotImage(int d, RplotImage *pximage, int *pwidth, int *pheight)
{
    SEXP dev = elt(findVar(install(".Devices"), R_BaseEnv), d);
#ifdef Win32
    HENHMETAFILE hemf;

    if (TYPEOF(dev) != STRSXP ||
	strcmp(CHAR(STRING_ELT(dev, 0)), "win.metafile:") != 0)
	return TCL_ERROR;

    killDevice(d);

    OpenClipboard(NULL);
    hemf = GetClipboardData(CF_ENHMETAFILE);
    CloseClipboard();
    if (hemf != NULL) {
	ENHMETAHEADER emh;
	GetEnhMetaFileHeader(hemf, sizeof(emh), &emh);
	*pximage = CopyEnhMetaFile(hemf, NULL);
	*pwidth = emh.rclBounds.right - emh.rclBounds.left;
	*pheight = emh.rclBounds.bottom - emh.rclBounds.top;
	return TCL_OK;
    }
    else return TCL_ERROR;
#else
    if (TYPEOF(dev) != STRSXP ||
	!(strcmp(CHAR(STRING_ELT(dev, 0)), "XImage") == 0 ||
	  strncmp(CHAR(STRING_ELT(dev, 0)), "PNG", 3) == 0 ||
	  strncmp(CHAR(STRING_ELT(dev, 0)), "X11", 3) == 0))
	return TCL_ERROR;

    if (R_GetX11Image(d, pximage, pwidth, pheight)) {
	killDevice(d);
        return TCL_OK;
    }
    else return TCL_ERROR;
#endif
}


/*
 *----------------------------------------------------------------------
 *
 * CreateRplot --
 *
 *	This procedure is called by the Tk image code to create "Rplot"
 *	images.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	The data structure for a new image is allocated.
 *
 *----------------------------------------------------------------------
 */
static int
CreateRplot(interp, name, argc, objv, typePtr, master, clientDataPtr)
    Tcl_Interp *interp;		/* interpreter that will own the image */
    char *name;			/* name to use for image */
    int argc;			/* number of arguments */
    Tcl_Obj *objv[];		/* argument strings  for options as objects
				   (doesn't include image name or type) */
    Tk_ImageType *typePtr;	/* not used */
    Tk_ImageMaster master;	/* token for image for use in callbacks */
    ClientData *clientDataPtr;	/* store pointer to master data data here */
{
    RplotMaster rpm, *prpm;

    /* check arguments */
    if (argc != 0) {
	Tcl_AppendResult(interp, "usage: image create Rplot ?name?", NULL);
	return TCL_ERROR;
    }

    /* get the device number */
    if (NoDevices()) {
	Tcl_AppendResult(interp, "no device active", NULL);
	return TCL_ERROR;
    }
    rpm.Rdevice = curDevice();

    /* get the image */
    if (GetRplotImage(rpm.Rdevice, &(rpm.image),
		      &(rpm.width), &(rpm.height)) != TCL_OK) {
	Tcl_AppendResult(interp, "can't get device image", NULL);
	return TCL_ERROR;
    }

    /* allocate and fill in the structure */
    prpm = (RplotMaster *) ckalloc(sizeof(RplotMaster)); /**** error check */
    prpm->interp = interp;
    prpm->Rdevice = rpm.Rdevice;
    prpm->haveImage = TRUE;
    prpm->image = rpm.image;
    prpm->width = rpm.width;
    prpm->height = rpm.height;
    prpm->instanceCount = 0;
    prpm->master = master;
    *clientDataPtr = (ClientData) prpm;
    return TCL_OK;
}


/*
 *----------------------------------------------------------------------
 *
 * GetRplot --
 *
 *	This procedure is called for each use of a Rplot image in a
 *	widget.
 *
 * Results:
 *	The return value is a token for the instance, which is passed
 *	back to us in calls to DisplayRplot and FreeRplot.
 *
 * Side effects:
 *	Allocates a data structure for the instance.
 *
 *----------------------------------------------------------------------
 */
static ClientData
GetRplot(tkwin, masterData)
    Tk_Window tkwin;		/* window for the instance */
    ClientData masterData;	/* pointer to master data structure */
{
    RplotMaster *prpm = (RplotMaster *) masterData;
    RplotInstance *inst;

    inst = (RplotInstance *) ckalloc(sizeof(RplotInstance)); /*** check */
    inst->master = prpm;
    inst->tkwin = tkwin;
    prpm->instanceCount++;

    /*
     * If this is the first instance, must set the size of the image.
     */
    if (prpm->instanceCount == 1) {
        int width = prpm->width;
	int height = prpm->height;
        Tk_ImageChanged(prpm->master, 0, 0, width, width, width, height);
    }

    return (ClientData) inst;
}


/*
 *----------------------------------------------------------------------
 *
 * DisplayRplot --
 *
 *	This procedure is invoked to draw a Rplot image.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	A portion of the image gets rendered in a pixmap or window.
 *
 *----------------------------------------------------------------------
 */
static void
DisplayRplot(clientData, display, drawable, imageX, imageY, width,
	height, drawableX, drawableY)
    ClientData clientData;	/* pointer to instance data structure */
    Display *display;		/* display on which to draw image */
    Drawable drawable;		/* pixmap or window in which to draw image. */
    int imageX, imageY;		/* upper-left corner of region within image */
    int width, height;		/* dimensions of region within image to draw */
    int drawableX, drawableY;	/* coordinates in drawable that correspond to
				   imageX and imageY. */
{
    RplotInstance *inst = (RplotInstance *) clientData;
    RplotMaster *prpm = inst->master;
#ifdef Win32
    TkWinDCState dcState;
    HDC hdc = TkWinGetDrawableDC(display, drawable, &dcState);
    RECT rect;

    SetRect(&rect, 0, 0, prpm->width, prpm->height);
    if (! PlayEnhMetaFile(hdc, prpm->image, &rect))
      REprintf("error code %d playing mefafile\n", GetLastError());
    TkWinReleaseDrawableDC(drawable, hdc, &dcState);
#else
    int depth = Tk_Depth(inst->tkwin);
    unsigned int gcMask;
    XGCValues gcValues;
    Pixmap pixmap;
    GC gc;

    pixmap = Tk_GetPixmap(display, Tk_WindowId(inst->tkwin),
			  width, height, depth);
    gc = Tk_GetGC(inst->tkwin, 0, NULL);
    XPutImage(display, pixmap, gc, prpm->image, 0, 0, 0, 0, width, height);
    Tk_FreeGC(display, gc);

    gcMask = GCGraphicsExposures;
    gcValues.graphics_exposures = False;
    gc = Tk_GetGC(inst->tkwin, gcMask, &gcValues);

    XSetClipOrigin(display, gc, drawableX - imageX, drawableY - imageY);
    XCopyArea(display, pixmap, drawable, gc, imageX, imageY,
	      (unsigned) width, (unsigned) height, drawableX, drawableY);
    XSetClipOrigin(display, gc, 0, 0);

    Tk_FreeGC(display, gc);
    Tk_FreePixmap(display, pixmap);
#endif
}


/*
 *----------------------------------------------------------------------
 *
 * FreeRplot --
 *
 *	This procedure is called when a widget ceases to use a
 *	particular instance of an image.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Internal data structures get cleaned up.
 *
 *----------------------------------------------------------------------
 */
static void
FreeRplot(clientData, display)
    ClientData clientData;	/* pointer instance data structure */
    Display *display;		/* display containing window that used image */
{
    RplotInstance *inst = (RplotInstance *) clientData;
    RplotMaster *prpm = inst->master;

    prpm->instanceCount--;
    ckfree((char *) inst);
}


/*
 *----------------------------------------------------------------------
 *
 * DeleteRplot --
 *
 *	This procedure is called by the image code to delete the
 *	master structure for an image.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Resources associated with the image get freed.
 *
 *----------------------------------------------------------------------
 */
static void
DeleteRplot(masterData)
    ClientData masterData;	/* pointer to master structure for image */
{
    RplotMaster *prpm = (RplotMaster *) masterData;

    if (prpm->instanceCount != 0)
	panic("tried to delete Rplot image when instances still exist");

    if (prpm->haveImage)
#ifdef Win32
	DeleteEnhMetaFile(prpm->image);
#else
	XDestroyImage(prpm->image);
#endif

    ckfree((char *) prpm);
}


/*
 *--------------------------------------------------------------
 *
 * Rplot_Init , Rplot_SafeInit --
 *	Create Rplot commands.
 *
 * Results:
 *	None
 *
 * Side effects:
 *	None
 *
 *--------------------------------------------------------------
 */

#ifndef USE_TCL_STUBS
#undef Tcl_InitStubs
#define Tcl_InitStubs(a,b,c) Tcl_PkgRequire(a,"Tcl",TCL_VERSION,0)
#endif

#ifndef USE_TK_STUBS
#undef Tk_InitStubs
#define Tk_InitStubs(a,b,c) Tcl_PkgRequire(a,"Tk",TK_VERSION,0)
#endif

#define RPLOT_PATCH_LEVEL "0.0.0"
#define EXPORT(a, b) a b

Tk_ImageType RplotImageType = {
    "Rplot",		/* name */
    CreateRplot,	/* createProc */
    GetRplot,		/* getProc */
    DisplayRplot,	/* displayProc */
    FreeRplot,		/* freeProc */
    DeleteRplot,	/* deleteProc */
    NULL		/* nextPtr */
};

EXPORT(int,Rplot_Init)(interp)
    Tcl_Interp *interp;
{
    static int initialized = 0;
    CONST char *version;

    if ((version = Tcl_InitStubs(interp, "8.0", 0)) == NULL)
	return TCL_ERROR;
    if (Tk_InitStubs(interp, "8.0", 0) == NULL)
	return TCL_ERROR;

    if (!initialized) {
	initialized = 1;
	Tk_CreateImageType(&RplotImageType);
    }
    return Tcl_PkgProvide(interp,"Rplot", RPLOT_PATCH_LEVEL);
}

EXPORT(int,Rplot_SafeInit)(interp)
    Tcl_Interp *interp;
{
    return Rplot_Init(interp);
}
