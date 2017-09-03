#include <stdio.h>
#include <string.h>
#include <stdio.h>

int main () {
  #define TAPE_SIZE 65536
  char mem[TAPE_SIZE] = {0};
  int ptr;

memcpy(&mem[ptr],(char[10]){0,0,0,1,0,0,0,0,0,0},10);ptr+=3;
while(mem[ptr]){
mem[ptr]=0;mem[ptr+2]=2;mem[ptr+3]+=1;mem[ptr+3]+=mem[ptr+4]*4;mem[ptr+3]+=28;mem[ptr+5]+=mem[ptr+4]*2;mem[ptr+4]=2;mem[ptr+5]+=14;mem[ptr+6]+=1;mem[ptr+7]+=1;mem[ptr+9]+=mem[ptr+8]*2;mem[ptr+8]+=5;mem[ptr+9]+=10;mem[ptr+10]+=mem[ptr+8]*6;mem[ptr+8]=1;ptr+=8;mem[ptr+3]=getchar();mem[ptr+2]+=2;ptr+=2;
while(mem[ptr]){

while(mem[ptr]){
ptr++;
while(mem[ptr]){
mem[ptr]--;ptr+=2;
}
ptr--;
while(mem[ptr]){
ptr+=2;
}
mem[ptr-2]-=1;ptr-=2;
}
ptr--;ptr-=(long)((void*)(mem+ptr)-memrchr(mem,0,ptr+1));mem[ptr-1]+=1;ptr++;ptr+=(long)(memchr(mem+ptr,0,sizeof(mem))-(void*)(mem+ptr));ptr++;
while(mem[ptr]){
mem[ptr]--;mem[ptr-1]+=1;
while(mem[ptr]){
mem[ptr-1]+=mem[ptr];mem[ptr]=0;ptr++;
}
ptr--;
while(mem[ptr]){

while(mem[ptr]){
mem[ptr]=0;ptr--;
}
mem[ptr]+=2;mem[ptr-1]-=1;ptr--;
while(mem[ptr]){
mem[ptr-1]+=9;mem[ptr-1]+=mem[ptr]*-1;mem[ptr]=0;ptr+=2;
}
ptr+=2;
}

}
ptr-=2;
}
ptr--;
}
ptr--;
while(mem[ptr]){
ptr-=(long)((void*)(mem+ptr)-memrchr(mem,0,ptr+1));ptr++;
while(mem[ptr]){
ptr+=(long)(memchr(mem+ptr,0,sizeof(mem))-(void*)(mem+ptr));ptr+=2;
while(mem[ptr]){
ptr+=2;
}
mem[ptr]++;
while(mem[ptr]){
ptr-=2;
}
ptr--;ptr-=(long)((void*)(mem+ptr)-memrchr(mem,0,ptr+1));mem[ptr-1]+=1;mem[ptr+1]-=1;ptr++;
}
ptr++;ptr+=(long)(memchr(mem+ptr,0,sizeof(mem))-(void*)(mem+ptr));mem[ptr]++;
while(mem[ptr]){
mem[ptr]--;ptr+=2;
}
ptr-=4;
while(mem[ptr]){

while(mem[ptr]){
ptr-=2;
}
ptr--;ptr-=(long)((void*)(mem+ptr)-memrchr(mem,0,ptr+1));mem[ptr]++;ptr-=2;
while(mem[ptr]){
mem[ptr]++;mem[ptr-1]-=1;mem[ptr+1]+=1;ptr--;
while(mem[ptr]){
mem[ptr]--;mem[ptr+1]-=2;mem[ptr+2]+=1;
while(mem[ptr]){
mem[ptr+1]+=1;mem[ptr+2]+=mem[ptr];mem[ptr]=0;
}

}
mem[ptr]+=mem[ptr+1];mem[ptr+1]=0;
}
mem[ptr]+=2;mem[ptr+2]-=2;ptr+=3;ptr+=(long)(memchr(mem+ptr,0,sizeof(mem))-(void*)(mem+ptr));ptr+=2;
while(mem[ptr]){
ptr+=2;
}

}
ptr-=2;
while(mem[ptr]){
mem[ptr+2]+=1;ptr++;
while(mem[ptr]){
ptr-=(long)((void*)(mem+ptr)-memrchr(mem,0,ptr+1));ptr--;
}
ptr++;
while(mem[ptr]){

while(mem[ptr]){
ptr-=2;
}
ptr--;ptr-=(long)((void*)(mem+ptr)-memrchr(mem,0,ptr+1));mem[ptr]++;
while(mem[ptr]){
mem[ptr]--;mem[ptr-1]+=1;mem[ptr+1]-=1;ptr++;
while(mem[ptr]){
mem[ptr]--;mem[ptr-2]+=1;mem[ptr-1]+=2;
while(mem[ptr]){
mem[ptr-1]-=1;mem[ptr-2]+=mem[ptr];mem[ptr]=0;
}

}
mem[ptr]+=mem[ptr-1];mem[ptr-1]=0;
}
ptr++;ptr+=(long)(memchr(mem+ptr,0,sizeof(mem))-(void*)(mem+ptr));ptr++;
}
ptr++;
while(mem[ptr]){
ptr+=2;
}
ptr+=2;
}
ptr-=2;
while(mem[ptr]){
mem[ptr+2]+=1;mem[ptr+4]+=1;ptr+=6;
}
ptr-=2;
while(mem[ptr]){
mem[ptr]--;ptr+=8;
}
ptr-=2;
while(mem[ptr]){
putchar(mem[ptr+1]);ptr+=8;
}
ptr-=2;
while(mem[ptr]){
mem[ptr+1]-=1;ptr+=6;
}
ptr-=2;
while(mem[ptr]){
mem[ptr+1]=getchar();ptr+=4;
}
ptr-=2;
while(mem[ptr]){
mem[ptr+1]+=1;ptr+=2;
}
ptr-=2;
while(mem[ptr]){
mem[ptr]++;ptr-=2;
}
ptr--;
}


  return 0;
}
