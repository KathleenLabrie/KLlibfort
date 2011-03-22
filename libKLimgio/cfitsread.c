/* For Fortran */
/* Function : cfitsread_
 *	Open a FITS file and write it to a buffer (line-by-line access).
 *	The buffer is provided by the user.
 *	Mirrors CJP's 'iraf_read_r4'.
 *
 */

#include <myutil.h>
#include <stdio.h>
#include <stddef.h>
#include <string.h>

#define TRUE 1
#define FALSE 0

int read_image( char filename[], double ***ptr2ppix, unsigned long naxes[] );

int cfitsread_ (char fname[], int *namelen, int ixysec[], int *ierimgsec, 
	float *buff, int *nmax, int *nx, int *ny, int *ier)
{
 int status, xmirror, ymirror;
 unsigned long int npixels;
 double **ppix=NULL;
 unsigned long naxes[2], ii, jj, k;
 char imgname[80];

 xmirror = ymirror = FALSE;

 strncpy(imgname,fname,*namelen);
 imgname[*namelen] = '\0';

 if (status = read_image(imgname, &ppix, naxes)) {
 	if (ppix != NULL) free_dmatrix(ppix);
	*ier=status;
 	printerror(status);
 }

 if (*ierimgsec == 1) {  /* take all the image */
 	ixysec[0] = 1;
	ixysec[1] = (int)naxes[0];
	ixysec[2] = 1;
	ixysec[3] = (int)naxes[1];
 } else if ( (ixysec[0] < 1) || (ixysec[0] > (int)naxes[0]) || /* X1 out of bounce */
 	      (ixysec[1] < 1) || (ixysec[1] > (int)naxes[0]) || /* X2 out of bounce */
	      (ixysec[2] < 1) || (ixysec[2] > (int)naxes[1]) || /* Y1 out of bounce */
 	      (ixysec[3] < 1) || (ixysec[3] > (int)naxes[1]) ) { /* Y2 out of bounce */
	printf("ERROR: Invalid section. Out of bounce.\n");
	*ier=-9998;
	return(*ier);
 }

 if (ixysec[0] > ixysec[1]) xmirror = TRUE;
 if (ixysec[2] > ixysec[3]) ymirror = TRUE;

 *nx = abs(ixysec[1]-ixysec[0]+1);
 *ny = abs(ixysec[3]-ixysec[2]+1);
 npixels = (*nx) * (*ny);
 if (npixels > *nmax) {
 	free_dmatrix(ppix);
	printf("ERROR: Image is too big\n");
	*ier=-9999;
	return(*ier);
 } 

 k=0;
 if (ymirror) {				/* Flip Y */
    for (jj=naxes[1]-1; jj>=0; jj--) {
      if (xmirror) {				/* Flip X */
         for (ii=naxes[0]-1; ii>=0; ii--) {
	    *(buff+k++) = (float)*(*(ppix+jj) + ii);
	  }
      } else {				/* Don't flip X */
	  for (ii=0; ii<naxes[0]; ii++) {
	    *(buff+k++) = (float)*(*(ppix+jj) + ii);
	  }
      }
    }
 } else {					/* Don't flip Y */
    for (jj=0; jj<naxes[1]; jj++) {
      if (xmirror) {				/* Flip X */
         for (ii=naxes[0]-1; ii>=0; ii--) {
	    *(buff+k++) = (float)*(*(ppix+jj) + ii);
	  }
      } else {				/* Don't flip X */
	  for (ii=0; ii<naxes[0]; ii++) {
	    *(buff+k++) = (float)*(*(ppix+jj) + ii);
	  }
      }
    }
 }

 free_dmatrix(ppix);

 return(0);
}
 
