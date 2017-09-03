#include <stdio.h>
#include <string.h>
int main () {
  #define TAPE_SIZE 65536
  char mem[TAPE_SIZE] = {0};
  int ptr;

ptr++;mem[ptr]++;ptr++;mem[ptr]++;ptr++;mem[ptr]++;ptr++;mem[ptr]++;ptr++;mem[ptr]+=2;ptr--;while(mem[ptr]){
  ptr++;while(mem[ptr]){
    ptr--;mem[ptr]+=3;ptr++;mem[ptr]--;ptr+=6;mem[ptr]++;ptr++;mem[ptr]++;ptr++;mem[ptr]++;ptr++;mem[ptr]++;ptr++;mem[ptr]+=2;ptr--;while(mem[ptr]){
      ptr++;while(mem[ptr]){
        ptr--;mem[ptr]+=3;ptr++;mem[ptr]--;ptr+=6;mem[ptr]++;ptr++;mem[ptr]++;ptr++;mem[ptr]++;ptr++;mem[ptr]++;ptr++;mem[ptr]+=2;ptr--;while(mem[ptr]){
          ptr++;while(mem[ptr]){
            ptr--;mem[ptr]+=3;ptr++;mem[ptr]--;ptr+=6;mem[ptr]++;ptr++;mem[ptr]++;ptr++;mem[ptr]++;ptr++;mem[ptr]++;ptr++;mem[ptr]+=2;ptr--;while(mem[ptr]){
              ptr++;while(mem[ptr]){
                mem[ptr-1]+=3;mem[ptr]--;mem[ptr+6]+=15; }mem[ptr+6]=0;
              ptr-=2;}
            ptr++;mem[ptr]=0;ptr-=5;}
          ptr-=2;}
        ptr++;mem[ptr]=0;ptr-=5;}
      ptr-=2;}
    ptr++;mem[ptr]=0;ptr-=5;}
  ptr-=2;}
ptr++;putchar(mem[ptr]);

  return 0;
}
