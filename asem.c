#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LINE_SIZE 1000
unsigned short mem[65536];
unsigned short pc,tabletop,startorg=0;
struct labtable {
   char name[16];
   unsigned short val;
};
struct labtable labtable[1000];

char line[MAX_LINE_SIZE+1]; // ptr to the current input line
char first[80],second[80],third[80],fourth[80],fifth[80];
char *label,*opcode,*dest,*src1,*src2;
FILE *input_file,*output_file;
int line_number = 0; 
    int pass; 
char * opcodes[]={"LOD","ADD","SUB","AND","ORA","XOR","SHR","MUL","STO","MIF",
"GTU","GTS","LTU","LTS","EQU","MAJ","WRD","ORG","TXT","CON"};

char * registers[]={"R0","R1","R2","R3","R4","R5","R6","R7","R8","R9",
"R10","R11","R12","R13","R14","R15"};

int getnum(char * dest) {
  unsigned short num,code;
  code=0;
  if (sscanf(dest,"%d",&num)==1) {
     num &= 0xFFFF;
     code=num;
  }
  else
  if (sscanf(dest,"$%x",&num)==1) {
     num &= 0xFFFF;
     code=num;
  }
  else 
    if (pass==2)
       printf("Wrong symbol %s at %d \n",dest,line_number);
  return code;

}


void myassemble (void) {
  char *next_field;  
  int i,op,dst,s1,s2,num,code;
  pc=0;
  line_number=0;
  while (1) {
    fgets(line, MAX_LINE_SIZE, input_file); // read the line from input_file
    if (feof(input_file))  
      break;
    line_number++;
    if (line[0]==0)
       continue;
    next_field=strstr(line,";");
    if (next_field)
      *next_field=0;
    next_field = strtok(line, " ,:;\t\n");
    first[0]=second[0]=third[0]=fourth[0]=fifth[0]=0;
    if (next_field) {
      strncpy(first,next_field,80);
      if (!strcmp(first,"TXT"))     
           next_field = strtok(NULL, "\t\n");  
      else
      next_field = strtok(NULL, " ,:;\t\n");
    }

    if (next_field) {
      strncpy(second,next_field,80);
      if (!strcmp(second,"TXT"))     
           next_field = strtok(NULL, "\t\n");  
      else
           next_field = strtok(NULL, " ,:;\t\n");
    }

    if (next_field) {
      strncpy(third,next_field,80);
      next_field = strtok(NULL, " ,:;\t\n");
    }

    if (next_field) {
      strncpy(fourth,next_field,80);
      next_field = strtok(NULL, " ,:;\t\n");
    }

    if (next_field) {
      strncpy(fifth,next_field,80);
    }
    op=-1;
    label=first;
    opcode=second;
    dest=third;
    src1=fourth;
    src2=fifth;
      
    for (i=0;i<=19;i++) {
      if (!strcmp(opcodes[i],second)) {
        op=i;
        break;
      }
      if (!strcmp(opcodes[i],first)) {
        label=NULL;
        opcode=first;
        dest=second;
        src1=third;
        src2=fourth;
        op=i;
        break;
      }
    }
    if (opcode[0] && op==-1 && pass==1)
       fprintf(stderr,"Wrong instruction in %d\n", line_number);
    for (i=0;i<=15;i++) {
       if (!strcmp(registers[i],dest))
         dst=i;
       if (!strcmp(registers[i],src1))
         s1=i;
       if (!strcmp(registers[i],src2))
         s2=i;
    }
    if (label !=NULL && pass==1 && strlen(label)>0) {
       strncpy(labtable[tabletop].name,label,16);
       if (op==19) {
          labtable[tabletop].val=getnum(dest);
       }
       else
          labtable[tabletop].val=pc;
//       printf("Added %s=%d\n",labtable[tabletop].name,labtable[tabletop].val);
       tabletop++;
    }
    if (op==-1 || op== 19) {
       code=0;
    }
    else
    if (op==16) {
       code=-1;
       for (i=tabletop;i>0;i--) {
          if (!strcmp(labtable[i-1].name,dest)) {
             code=labtable[i-1].val & 0xFFFF;
             break;
          }
       }
       if (code==-1) {
          code=getnum(dest);
       }
       mem[pc++]=code;

    }
    else
    if (op==17) {
       num=getnum(dest);
       num &= 0xFFFF;
       pc=num;
       if (startorg==0)
          startorg=pc;
    }
    else
    if (op==18) {
       num=strlen(dest);
       if (num==0 || dest[0] !='"' || dest[num-1] !='"')
          fprintf(stderr,"Invalid text %s at %d\n",dest,line_number);
       for (i=1;i<num-1;i++)
          mem[pc++]=dest[i];
    }
    else
    {
       code=op<<12 | dst <<8 | s1<<4 | s2;
       mem[pc++]=code;

    }

//    printf("-%s-%s-%s-%s-%s-%04X\n",label,opcode,dest,src1,src2,code);
  }
}

void main(int argc, char *argv[]) {
  int i;
  tabletop=0;
  if (argc != 3) {
    fprintf(stderr, "usage: %s input_file output_file\n", argv[0]);
    exit(1);
  }
  for (pass=1;pass<=2;pass++) {
    input_file = fopen(argv[1], "r");
    if (input_file == NULL) {
      perror(argv[1]);
      exit(1);
    }
    myassemble();
  }
  fclose(input_file);
  output_file = fopen (argv[2], "wb");
  fwrite (mem , sizeof(short), pc-startorg, output_file);
  fclose (output_file);

  output_file = fopen ("table.txt", "wb");
  for (i=tabletop;i>0;i--) {
     fprintf(output_file,"%s %4x \n",labtable[i-1].name, labtable[i-1].val);
  }
  fclose (output_file);

}