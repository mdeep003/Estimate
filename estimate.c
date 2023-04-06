#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

//free variables
void MFree(int varint, double** mar)
{
    int randvarcheck2 =0;
    double* current;
    for(randvarcheck2 = 0; randvarcheck2 < varint; randvarcheck2++)
    {
        current = mar[randvarcheck2];
        free(current);
        current = NULL;
    }
    free(mar);
    mar = NULL;
}

//printing Matrix 
void printM(int n, int startingvarY, double **mar){
        int i;
        int middleX; 
        for(i = 0; i < n; i++){
                for(middleX = 0; middleX < startingvarY; middleX++){
                        printf("%0.0lf", mar[i][middleX]);
                }
                printf("\n");
        }
        return;
}

double** multimatrix(double**mat,int row,int col,double**mat1,int row1,int col1){

    if(col != row1)
    {
      return NULL;
    }

    else 
    {
        double** result;
        result = (double**) malloc(row*sizeof(double*));

        for(int i=0;i<row;i++)
        {
            result[i] = (double*)malloc(col1*sizeof(double));
        }

        for(int i=0;i<row;i++)
        {
            for(int middleX=0;middleX<col1;middleX++)
            {

                result[i][middleX]=0;

                for(int startingvarY=0;startingvarY<col;startingvarY++)
                {
                result[i][middleX]+= (mat[i][startingvarY]*mat1[startingvarY][middleX]);
                }

            }

        }
      return result;
  }

}


double** transpMat(double **mat, int first, int second)
{
  int starterN;
  int finisherY;
  double **matrixNew = NULL;


  matrixNew = (double**) malloc(sizeof(double*) * second);




  for(starterN=0; starterN<second ; starterN++)
  {
    matrixNew[starterN] = (double*) malloc(sizeof(double) * first);
  }

  for(starterN=0; starterN<first; starterN++)
  {
    for(finisherY=0; finisherY<second ;finisherY++)
    {

      matrixNew[finisherY][starterN] =
       mat[starterN][finisherY];
    }
  }
  return matrixNew;
}

double** matrixfuture(double** fut1, double** fut2, int first, int second)
{
    int starterY; 
   
    double** estMatrix;

    estMatrix=(double**) malloc(first* sizeof(double*));

    for (starterY = 0; starterY < first; starterY++)
    {
        estMatrix[starterY]=(double*) malloc(1* sizeof(double));
    }


    int zeronum =0; 
     int finisherX;

    for(starterY = 0; starterY < first; starterY++)
    {
        for(finisherX = 0; finisherX < second ;finisherX++)
        {
            if(zeronum == finisherX)
            {
                estMatrix[starterY][0] = fut2[finisherX][0];
                continue;
            }
            else
            {
                estMatrix[starterY][0] = estMatrix[starterY][0] + ( fut2[finisherX][0] *  fut1[starterY][finisherX-1]);
                continue;
            }
        }
    }
    return estMatrix;
}


double** invMatrix(double** matrix, int newvar)
{
    //all the varb
    int count = newvar;
    double counter; 
    double columnsD= newvar*2;

    int startingvarY;
    int endingZ;
    int starterY;
    int middleX;
    

    double ** inverseM;

        inverseM = (double**) malloc(sizeof(double*) * newvar);

        for (starterY = 0; starterY <newvar; starterY++) 
        { 
            inverseM[starterY] = (double*) malloc(sizeof(double) * columnsD);
        }


//new matrix (final)
    double ** matrixredo;
    matrixredo = (double **) malloc (newvar * sizeof(double*));

     for (starterY = 0; starterY < newvar; starterY++) 
    {                
        matrixredo[starterY] = (double*) malloc (newvar * sizeof(double));
    }

//keeping track of the inv numdubs (large numdubs)
double invnum = 1.000000; 
double invnumzero =0.000000;

    for(starterY = 0; starterY < newvar; starterY++)
    {
        for(middleX = 0; middleX < (columnsD); middleX++)
        {
            if(count == middleX)
            {
                inverseM[starterY][middleX] = invnum;
            }
            else if(middleX > (newvar-1))
            {
                inverseM[starterY][middleX] = invnumzero;
            }
            else{
                inverseM[starterY][middleX] = matrix[starterY][middleX];
            }
        }
        count++;
    }

    for(starterY = 0; starterY < newvar; starterY++)
    {
        for(middleX = 0; middleX < newvar; middleX++)
        {   
            if(middleX < starterY)
            {
                continue;
            }

            int zeroval=0; 
            if( (invnum != inverseM[starterY][middleX] ) && (middleX==starterY))
            { 
                   counter = inverseM[starterY][middleX]; 
                   for(startingvarY = zeroval; startingvarY < (columnsD); startingvarY++)
                   {
                       inverseM[starterY][startingvarY] = inverseM[starterY][startingvarY] / counter;
                   }

                   continue;
            }
            else if((invnumzero != inverseM[middleX][starterY]) && (starterY!=middleX))
            {
                for(endingZ = middleX; endingZ < newvar; endingZ++)
                {   
                    counter = inverseM[endingZ][starterY];
                    for(startingvarY = zeroval; startingvarY < (columnsD); startingvarY++)
                    {
                        inverseM[endingZ][startingvarY] = inverseM[endingZ][startingvarY] + ((-1*counter)* inverseM[middleX-1][startingvarY]);
                    }
                }
                continue;
            }
        }
    }

    int negval=-1; 
    int zeroag=0;
    for(starterY = (newvar-2); starterY > negval; starterY--)
    {
        for(middleX = (newvar-1); inverseM[starterY][middleX] != invnum; middleX--){

            for(endingZ = starterY; endingZ > negval; endingZ--)
            {
                counter = inverseM[starterY][middleX];
                for(startingvarY = zeroag; startingvarY < (columnsD); startingvarY++)
                {
                    inverseM[endingZ][startingvarY] = inverseM[endingZ][startingvarY] + ((negval*counter)* inverseM[middleX][startingvarY]);
                }
            }
        }
    }
    for(starterY = zeroag; starterY < newvar; starterY++){
        for(middleX = zeroag; middleX < newvar; middleX++){
            matrixredo[starterY][middleX] = inverseM[starterY][middleX+newvar];
        }
    }


    MFree(newvar, inverseM);
    return matrixredo; 
}



int main(int argc, char *argv[]){

    int zeroval=0;
    int threevar=3; 
        if (threevar != argc)
        {
            printf("error\n");
            exit(zeroval);
        }

        FILE *fileprinter = fopen(argv[1], "r");

        if(fileprinter==NULL)
        {
            exit(zeroval);
        }

        FILE *pointerfile = fopen(argv[2], "r");
        if(pointerfile == NULL)
        {        
            exit(zeroval);
        }

        char charavar[10];
        int startingvarY;
        int sidevarP1;

        int beginning;
        int newbe;
        int middleX;
        double numdub;
        
        fscanf(fileprinter, "%s\n", charavar);
        fscanf(pointerfile, "%s\n", charavar);

        fscanf(fileprinter, "%d\n", &startingvarY);
        fscanf(pointerfile, "%d\n", &sidevarP1);
        
        fscanf(fileprinter, "%d\n", &beginning);

        fscanf(pointerfile, "%d\n", &newbe);

        double** dubmatvar;

                dubmatvar = (double**)malloc(sizeof(double*)* beginning);
                int randvarcheck;

                for(randvarcheck=(zeroval);randvarcheck<(beginning);randvarcheck++){

           dubmatvar[randvarcheck] = (double*)malloc(sizeof(double) * (startingvarY+1));

      }
      int numone=1; 
      double** dubmatvar2;
                dubmatvar2 = (double**)malloc(sizeof(double*) * beginning);
                int randvarcheck2;
                for(randvarcheck2=(zeroval);randvarcheck2<(beginning);randvarcheck2++){

           dubmatvar2[randvarcheck2] = (double*)malloc(sizeof(double) * numone);

      }
      double** DataMart;
                DataMart = (double**)malloc(sizeof(double*) * newbe);
                int newvar3;
                for(newvar3=zeroval;newvar3<newbe;newvar3++){

           DataMart[newvar3] = (double*)malloc(sizeof(double) * sidevarP1);

      }
        for(int i = zeroval; i < beginning; i++){
                for(middleX = zeroval; middleX < startingvarY+2; middleX++){
                        if(zeroval == middleX){     
                                dubmatvar[i][middleX] = 1.0;
                                if(newbe > i){
                                        fscanf(pointerfile, "%lf ", &numdub);
                                        // checker 
                                        DataMart[i][middleX] = numdub;
                                }
                        }
                        else if((startingvarY+1) == middleX){
                                fscanf(fileprinter, "%lf\n", &numdub);
                                dubmatvar2[i][zeroval] = numdub;
                        }else{
                            int onevar =1;
                                fscanf(fileprinter, "%lf ", &numdub);
                                dubmatvar[i][middleX] = numdub;
                                if((middleX == (sidevarP1-onevar)) && (newbe > i) ){ 
                                        fscanf(pointerfile, "%lf\n", &numdub);
                                        DataMart[i][middleX] = numdub;                            
                                }
                                else if((middleX < sidevarP1) && (newbe > i)){ 
                                        fscanf(pointerfile, "%lf ", &numdub);
                    DataMart[i][middleX] = numdub;
                                }
                        }
                }
        }
        fclose(fileprinter); fclose(pointerfile);

        int starv2 = startingvarY +1;

        double **transpMatMatrix = transpMat(dubmatvar, beginning, starv2);
        double **multMatrix = multimatrix(transpMatMatrix, (starv2), beginning, dubmatvar, beginning, (starv2));
        double **invMatrixMatrix = invMatrix( multMatrix,  starv2);
        double **fixerextra = multimatrix(invMatrixMatrix, starv2, starv2, transpMatMatrix, starv2, beginning);
        double **wdouble = multimatrix(fixerextra, starv2, beginning, dubmatvar2, beginning, 1);
        double **housedub = matrixfuture(DataMart, wdouble, newbe, starv2);
        printM(newbe, 1, housedub);

//final matrix v var check 
    MFree(beginning, dubmatvar);
    MFree(beginning, dubmatvar2);
    MFree(newbe, DataMart);


    MFree(starv2, transpMatMatrix);
    MFree( starv2, multMatrix);
    MFree(starv2, invMatrixMatrix);
    MFree(starv2, fixerextra);
    MFree(starv2, wdouble);


    MFree( newbe, housedub);
    //return 0 at the end
        return 0;
}