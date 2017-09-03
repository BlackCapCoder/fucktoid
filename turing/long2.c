#include <stdio.h>
#include <string.h>
int main () {
  #define TAPE_SIZE 65536
  char mem[TAPE_SIZE] = {0};
  int ptr;

mem[ptr+1]+=1;mem[ptr+2]+=1;mem[ptr+3]+=1;mem[ptr+4]+=1;mem[ptr+5]+=2;ptr+=4;
while(mem[ptr]){
  ptr++;
  while(mem[ptr]){
    mem[ptr]--;mem[ptr-1]+=3;mem[ptr+6]+=1;mem[ptr+7]+=1;mem[ptr+8]+=1;mem[ptr+9]+=1;mem[ptr+10]+=2;ptr+=9;
    while(mem[ptr]){
      ptr++;
      while(mem[ptr]){
        mem[ptr]--;mem[ptr-1]+=3;mem[ptr+6]+=1;mem[ptr+7]+=1;mem[ptr+8]+=1;mem[ptr+9]+=1;mem[ptr+10]+=2;ptr+=9;
        while(mem[ptr]){
          ptr++;
          while(mem[ptr]){
            mem[ptr]--;mem[ptr-1]+=3;mem[ptr+6]+=1;mem[ptr+7]+=1;mem[ptr+8]+=1;mem[ptr+9]+=1;mem[ptr+10]+=2;ptr+=9;
            while(mem[ptr]){
              ptr++;
              while(mem[ptr]){
                mem[ptr]--;mem[ptr-1]+=3;mem[ptr+5]+=3;mem[ptr+6]+=mem[ptr+5]*5;mem[ptr+5]=0;mem[ptr+6]=0;
              }
              ptr-=2;
            }
            mem[ptr+1]=0;ptr-=4;
          }
          ptr-=2;
        }
        mem[ptr+1]=0;ptr-=4;
      }
      ptr-=2;
    }
    mem[ptr+1]=0;ptr-=4;
  }
  ptr-=2;
}
ptr++;putchar(mem[ptr]);

  return 0;
}
