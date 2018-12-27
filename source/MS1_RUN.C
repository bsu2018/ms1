/* MS1_RUN */
#include <stdio.h>
#include <fcntl.h>
#include <sys\types.h>
#include <sys\stat.h>
#include <io.h>
#include <process.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
static char *arg[4];
int main( int argc, char *argv [] ) {
       int   i,j, rc;
  static char NAME[10]="        ";
  static
  int oflag    =  O_TEXT|O_TRUNC|O_CREAT|O_WRONLY ;
  static
  int pmode =  S_IWRITE;
  int handler;
  if (argc != 2 ) { fprintf(stderr,"\n   ***** MS1 *****\n");
                    fprintf(stderr,"\n Необходимо указывать: \n");
                    fprintf(stderr,"\n MS1_RUN  <Имя работы>\n");
                    exit(1);
                 };
  strcpy(NAME,argv[1]); // +job.....
  if((handler   =open("MS1_DATA.$$$",oflag,pmode)) ==-1 )
        { fprintf(stderr,"\n Невоэможно открыть файл MS1_DATA.$$$\n");
          exit(1);
        };
   strcat(NAME,"\n");

   if( write(handler   ,NAME, strlen(NAME)) == -1)
        { fprintf(stderr,"\n Невоэможно создать файл MS1_DATA.$$$\n");
          exit(1);
        };
   if( write(handler   ,"0\n", 2) == -1)
        { fprintf(stderr,"\n Невоэможно писать в файл MS1_DATA.$$$\n");
          exit(1);
        };
   close (handler);
/********************************/
  while ( 1 )
    {
       arg[0]=(char *) "..\\bin\\MS1ROOT.EXE";
       arg[1]=NULL;
       rc=spawnvp(P_WAIT, "..\\bin\\MS1ROOT.EXE", arg );
       printf("\n MS1ROOT.EXE rc=%d   \n",rc);  
       if(rc ==-1)
          { perror("\nОшибка вызова процесса MS1ROOT: ");
            exit(666);
           };
       /****/
      if( rc == 4 )
         {
    /******arg[0]="\\MS1\\MS1GETDM.EXE";
           arg[1]=NULL;
           rc=spawnvp(P_WAIT, "..\\bin\\MS1GETDM.EXE", arg );
           if(rc ==-1)
              { perror("\nОшибка вызова процесса MS1GETDM: ");
                exit(666);
              };
   *****/
            arg[0]=NULL; // "\\MS1\\MS1CIRCT.EXE";
           arg[1]=NULL;
           rc=spawnvp(P_WAIT, "..\\bin\\MS1CIRCT.EXE", arg );
           printf("\n MS1CIRCT.EXE rc=%d   \n",rc);  
           if(rc ==-1)
              { perror("\nОшибка вызова процесса MS1CIRCT: ");
                exit(666);
               };
         };
       /****/
       /****/
      if( rc == 5 )
         {
           arg[0]=NULL; //"\\MS1\\MS1GETDM.EXE";
           arg[1]=NULL;
           rc=spawnvp(P_WAIT, "..\\bin\\MS1GETDM.EXE", arg );
           printf("\n MS1GETDM.EXE rc=%d   \n",rc);   
           if(rc ==-1)
              { perror("\nОшибка вызова процесса MS1GETDM: ");
                exit(666);
              };
           arg[0]=NULL; // "\\MS1\\MS1DIGIT.EXE";
           arg[1]=NULL;
           rc=spawnvp(P_WAIT, "..\\bin\\MS1DIGIT.EXE", arg );
            printf("\n MS1DIGIT.EXE.EXE rc=%d   \n",rc);   
           if(rc ==-1)
              { perror("\nОшибка вызова процесса MS1DIGIT: ");
                exit(666);
               };
         };
       /****/
       if( rc == 6 ) continue;
       if(rc != 5)
           { fprintf(stderr,"\nК о н е ц   р а б о т ы\n");
             exit(0);
            };
    };
}
