#include <stdio.h>
#include <string.h>
#include <stdio.h>

int main () {
  #define TAPE_SIZE 65536
  char mem[TAPE_SIZE] = {0};
  int ptr;

memcpy(&mem[ptr],(char[10]){13,0,0,0,0,0,0,0,0,0},10);mem[ptr+1]+=mem[ptr]*2;mem[ptr+4]+=mem[ptr]*5;mem[ptr+5]+=mem[ptr]*2;mem[ptr+6]+=mem[ptr];mem[ptr]=0;mem[ptr+5]+=6;mem[ptr+6]-=3;mem[ptr+16]+=15;ptr+=16;
while(mem[ptr]){

while(mem[ptr]){
ptr+=9;
}
mem[ptr]++;
while(mem[ptr]){
ptr-=9;
}
mem[ptr+9]-=1;ptr+=9;
}
mem[ptr]++;
while(mem[ptr]){
mem[ptr+8]=0;ptr+=9;
}
ptr-=9;
while(mem[ptr]){
ptr-=9;
}
mem[ptr+1]+=5;mem[ptr+8]=1;ptr++;
while(mem[ptr]){
mem[ptr]--;mem[ptr+9]+=mem[ptr];mem[ptr]=0;ptr+=9;
}
mem[ptr+7]+=1;mem[ptr+34]+=1;ptr+=17;
while(mem[ptr]){
ptr-=9;
}
mem[ptr+3]=1;ptr+=3;
while(mem[ptr]){
ptr+=6;
while(mem[ptr]){
mem[ptr+7]=0;ptr+=9;
}
ptr-=9;
while(mem[ptr]){
ptr-=9;
}
mem[ptr+1]+=4;mem[ptr+7]=1;ptr++;
while(mem[ptr]){
mem[ptr]--;mem[ptr+9]+=mem[ptr];mem[ptr]=0;ptr+=9;
}
mem[ptr]+=7;mem[ptr+6]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr+9]+=mem[ptr];mem[ptr]=0;ptr+=9;
}
mem[ptr+6]+=1;ptr-=10;
while(mem[ptr]){
ptr-=9;
}
ptr+=3;
while(mem[ptr]){
mem[ptr]=0;ptr+=6;
while(mem[ptr]){
mem[ptr+1]+=mem[ptr+7];mem[ptr+7]=0;mem[ptr+2]+=mem[ptr+1];mem[ptr+5]+=mem[ptr+1];mem[ptr+7]+=mem[ptr+1];mem[ptr+1]=0;ptr+=9;
}
ptr-=9;
while(mem[ptr]){
ptr-=9;
}
ptr+=9;
while(mem[ptr]){
mem[ptr+1]+=mem[ptr+8];mem[ptr+8]=0;mem[ptr+3]+=mem[ptr+1];mem[ptr+6]+=mem[ptr+1];mem[ptr+8]+=mem[ptr+1];mem[ptr+1]=0;ptr+=9;
}
ptr-=9;
while(mem[ptr]){
ptr-=9;
}
mem[ptr]+=mem[ptr+7];mem[ptr+7]=0;mem[ptr+5]+=mem[ptr];mem[ptr+7]+=mem[ptr];mem[ptr]=0;mem[ptr+9]+=15;ptr+=9;
while(mem[ptr]){

while(mem[ptr]){
ptr+=9;
}
mem[ptr]++;memcpy(&mem[ptr+1],(char[9]){0,0,0,0,0,0,0,0,0},9);
while(mem[ptr]){
ptr-=9;
}
mem[ptr+9]-=1;ptr+=9;
}
mem[ptr]++;
while(mem[ptr]){
mem[ptr+1]+=1;ptr+=9;
}
ptr-=9;
while(mem[ptr]){
ptr-=9;
}
ptr+=9;
while(mem[ptr]){
mem[ptr+1]-=1;mem[ptr+1]+=mem[ptr+5];mem[ptr+5]=0;ptr++;
while(mem[ptr]){
mem[ptr]--;mem[ptr+4]+=1;ptr--;
while(mem[ptr]){
mem[ptr]--;mem[ptr]+=mem[ptr+2];mem[ptr+2]=mem[ptr];mem[ptr+4]+=mem[ptr];mem[ptr]=0;mem[ptr]++;ptr+=9;
}
ptr-=8;
while(mem[ptr]){
ptr-=9;
}

}
ptr+=9;
while(mem[ptr]){
ptr+=9;
}
ptr-=9;
while(mem[ptr]){
mem[ptr+10]+=mem[ptr+1];mem[ptr+1]=0;ptr-=9;
}
mem[ptr+10]+=mem[ptr+1];mem[ptr+1]=0;mem[ptr]++;ptr+=8;
}
ptr-=9;
while(mem[ptr]){
mem[ptr+1]=0;mem[ptr]--;ptr+=4;
while(mem[ptr]){
mem[ptr]--;mem[ptr-4]+=1;mem[ptr-9]+=mem[ptr-3];mem[ptr-4]+=mem[ptr-3]*-1;mem[ptr-3]=mem[ptr-4];mem[ptr-4]=0;
}
mem[ptr]+=mem[ptr-3];mem[ptr-4]+=1;mem[ptr-3]=0;ptr-=13;
}
ptr+=9;
while(mem[ptr]){
mem[ptr+1]+=1;ptr+=9;
}
ptr-=9;
while(mem[ptr]){
ptr-=9;
}
ptr+=9;
while(mem[ptr]){
mem[ptr+1]-=1;mem[ptr+1]+=mem[ptr+6];mem[ptr+6]=0;ptr++;
while(mem[ptr]){
mem[ptr]--;mem[ptr+5]+=1;ptr--;
while(mem[ptr]){
mem[ptr]--;mem[ptr]+=mem[ptr+3];mem[ptr+3]=mem[ptr];mem[ptr+4]+=mem[ptr];mem[ptr]=0;mem[ptr]++;ptr+=9;
}
ptr-=8;
while(mem[ptr]){
ptr-=9;
}

}
ptr+=9;
while(mem[ptr]){
ptr+=9;
}
ptr-=9;
while(mem[ptr]){
mem[ptr+11]+=mem[ptr+2];mem[ptr+2]=0;ptr-=9;
}
mem[ptr+11]+=mem[ptr+2];mem[ptr+2]=0;mem[ptr]++;ptr+=8;
}
ptr-=9;
while(mem[ptr]){
mem[ptr+1]=0;mem[ptr]--;ptr+=4;
while(mem[ptr]){
mem[ptr]--;mem[ptr-4]+=1;mem[ptr-9]+=mem[ptr-3];mem[ptr-4]+=mem[ptr-3]*-1;mem[ptr-3]=mem[ptr-4];mem[ptr-4]=0;
}
mem[ptr]+=mem[ptr-3];mem[ptr-4]+=1;mem[ptr-3]=0;ptr-=13;
}
ptr+=9;
while(mem[ptr]){
mem[ptr-32]+=mem[ptr+4];mem[ptr+4]=0;ptr+=9;
}
ptr-=9;
while(mem[ptr]){
ptr-=9;
}
mem[ptr+9]+=15;ptr+=9;
while(mem[ptr]){

while(mem[ptr]){
ptr+=9;
}
mem[ptr-9]-=1;ptr-=18;
while(mem[ptr]){
ptr-=9;
}
mem[ptr+9]-=1;ptr+=9;
}
mem[ptr]++;mem[ptr+21]+=1;ptr+=18;
while(mem[ptr]){
ptr-=9;
}
ptr+=9;
while(mem[ptr]){
mem[ptr]+=mem[ptr+3]*-1;mem[ptr+3]=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr+3]-=1;mem[ptr]+=mem[ptr+4];mem[ptr+4]=0;
while(mem[ptr]){
mem[ptr]--;mem[ptr+4]+=1;ptr-=9;
while(mem[ptr]){
ptr-=9;
}
mem[ptr+4]=1;ptr+=9;
while(mem[ptr]){
ptr+=9;
}
mem[ptr+1]+=1;
}

}
mem[ptr]++;mem[ptr]+=mem[ptr+4]*-1;mem[ptr+4]=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr+4]-=1;mem[ptr]+=mem[ptr+3];mem[ptr+3]=0;
while(mem[ptr]){
mem[ptr]--;mem[ptr+3]+=1;ptr-=9;
while(mem[ptr]){
ptr-=9;
}
mem[ptr+3]=1;ptr+=9;
while(mem[ptr]){
ptr+=9;
}
mem[ptr+1]=1;
}

}
mem[ptr]++;ptr++;
while(mem[ptr]){
mem[ptr]--;ptr--;
while(mem[ptr]){
ptr+=9;
}
ptr-=8;
}
ptr+=8;
}
ptr-=9;
while(mem[ptr]){
ptr-=9;
}
mem[ptr-6]+=mem[ptr-7];mem[ptr-3]+=mem[ptr-7]*-1;mem[ptr-7]=0;mem[ptr+2]+=26;mem[ptr]+=mem[ptr+4];mem[ptr+4]=0;
while(mem[ptr]){
mem[ptr]--;mem[ptr+2]=0;mem[ptr+4]+=1;
}
ptr+=2;
while(mem[ptr]){
mem[ptr-7]+=1;ptr-=8;
while(mem[ptr]){
mem[ptr]--;mem[ptr-1]+=1;mem[ptr+1]=0;mem[ptr+3]+=1;ptr++;
}
ptr++;
while(mem[ptr]){
mem[ptr]--;mem[ptr-1]+=mem[ptr-2];mem[ptr+2]+=mem[ptr-2]*-1;mem[ptr-2]=0;ptr++;
}
ptr+=13;
while(mem[ptr]){
memcpy(&mem[ptr+2],(char[3]){0,0,0},3);ptr+=9;
}
ptr-=9;
while(mem[ptr]){
ptr-=9;
}
mem[ptr+3]=0;ptr+=9;
while(mem[ptr]){
mem[ptr+1]+=mem[ptr+5];mem[ptr+5]=0;mem[ptr+2]+=mem[ptr+1];mem[ptr+5]+=mem[ptr+1];mem[ptr+1]=0;ptr+=9;
}
ptr-=9;
while(mem[ptr]){
ptr-=9;
}
ptr+=9;
while(mem[ptr]){
mem[ptr-7]+=mem[ptr+2];mem[ptr+2]=0;ptr+=9;
}
ptr-=9;
while(mem[ptr]){
ptr-=9;
}
mem[ptr+9]+=15;ptr+=9;
while(mem[ptr]){

while(mem[ptr]){
ptr+=9;
}
mem[ptr]++;memcpy(&mem[ptr+1],(char[9]){0,0,0,0,0,0,0,0,0},9);
while(mem[ptr]){
ptr-=9;
}
mem[ptr+9]-=1;ptr+=9;
}
mem[ptr]++;
while(mem[ptr]){
mem[ptr+1]+=1;ptr+=9;
}
ptr-=9;
while(mem[ptr]){
ptr-=9;
}
ptr+=9;
while(mem[ptr]){
mem[ptr+1]-=1;mem[ptr+1]+=mem[ptr+6];mem[ptr+6]=0;ptr++;
while(mem[ptr]){
mem[ptr]--;mem[ptr+5]+=1;ptr--;
while(mem[ptr]){
mem[ptr]--;mem[ptr]+=mem[ptr+2];mem[ptr+2]=mem[ptr];mem[ptr+3]+=mem[ptr];mem[ptr]=0;mem[ptr]++;ptr+=9;
}
ptr-=8;
while(mem[ptr]){
ptr-=9;
}

}
ptr+=9;
while(mem[ptr]){
ptr+=9;
}
ptr-=9;
while(mem[ptr]){
mem[ptr+10]+=mem[ptr+1];mem[ptr+1]=0;ptr-=9;
}
mem[ptr+10]+=mem[ptr+1];mem[ptr+1]=0;mem[ptr]++;ptr+=8;
}
ptr-=9;
while(mem[ptr]){
mem[ptr+1]=0;mem[ptr]--;ptr+=3;
while(mem[ptr]){
mem[ptr]--;mem[ptr-3]+=1;mem[ptr-9]+=mem[ptr-2];mem[ptr-3]+=mem[ptr-2]*-1;mem[ptr-2]=mem[ptr-3];mem[ptr-3]=0;
}
mem[ptr]+=mem[ptr-2];mem[ptr-3]+=1;mem[ptr-2]=0;ptr-=12;
}
ptr+=9;
while(mem[ptr]){
mem[ptr+1]+=mem[ptr+6];mem[ptr+6]=0;mem[ptr+2]+=mem[ptr+1];mem[ptr+6]+=mem[ptr+1];mem[ptr+1]=0;ptr+=9;
}
ptr-=9;
while(mem[ptr]){
ptr-=9;
}
ptr+=9;
while(mem[ptr]){
mem[ptr+1]+=1;ptr+=9;
}
ptr-=9;
while(mem[ptr]){
ptr-=9;
}
ptr+=9;
while(mem[ptr]){
mem[ptr+1]-=1;mem[ptr+1]+=mem[ptr+6];mem[ptr+6]=0;ptr++;
while(mem[ptr]){
mem[ptr]--;mem[ptr+5]+=1;ptr--;
while(mem[ptr]){
mem[ptr]--;mem[ptr]+=mem[ptr+2];mem[ptr+2]=mem[ptr];mem[ptr+4]+=mem[ptr];mem[ptr]=0;mem[ptr]++;ptr+=9;
}
ptr-=8;
while(mem[ptr]){
ptr-=9;
}

}
ptr+=9;
while(mem[ptr]){
ptr+=9;
}
ptr-=9;
while(mem[ptr]){
mem[ptr+10]+=mem[ptr+1];mem[ptr+1]=0;ptr-=9;
}
mem[ptr+10]+=mem[ptr+1];mem[ptr+1]=0;mem[ptr]++;ptr+=8;
}
ptr-=9;
while(mem[ptr]){
mem[ptr+1]=0;mem[ptr]--;ptr+=4;
while(mem[ptr]){
mem[ptr]--;mem[ptr-4]+=1;mem[ptr-9]+=mem[ptr-3];mem[ptr-4]+=mem[ptr-3]*-1;mem[ptr-3]=mem[ptr-4];mem[ptr-4]=0;
}
mem[ptr]+=mem[ptr-3];mem[ptr-4]+=1;mem[ptr-3]=0;ptr-=13;
}
ptr+=9;
while(mem[ptr]){
mem[ptr-32]+=mem[ptr+4];mem[ptr+4]=0;ptr+=9;
}
ptr-=9;
while(mem[ptr]){
ptr-=9;
}
ptr+=9;
while(mem[ptr]){
mem[ptr-33]+=mem[ptr+3];mem[ptr+3]=0;ptr+=9;
}
ptr-=9;
while(mem[ptr]){
ptr-=9;
}
mem[ptr+9]+=15;ptr+=9;
while(mem[ptr]){

while(mem[ptr]){
ptr+=9;
}
mem[ptr-9]-=1;ptr-=18;
while(mem[ptr]){
ptr-=9;
}
mem[ptr+9]-=1;ptr+=9;
}
mem[ptr]++;
while(mem[ptr]){
mem[ptr+1]+=mem[ptr+8];mem[ptr+8]=0;mem[ptr+2]+=mem[ptr+1];mem[ptr+8]+=mem[ptr+1];mem[ptr+1]=0;ptr+=9;
}
ptr-=9;
while(mem[ptr]){
ptr-=9;
}
ptr+=9;
while(mem[ptr]){
mem[ptr+6]=0;ptr+=9;
}
ptr-=9;
while(mem[ptr]){
ptr-=9;
}
mem[ptr+4]+=1;mem[ptr]+=mem[ptr+5];mem[ptr+4]+=mem[ptr+5]*-1;mem[ptr+5]=0;ptr+=6;
while(mem[ptr]){
mem[ptr]--;mem[ptr-2]+=mem[ptr-6]*2;mem[ptr-1]+=mem[ptr-6];mem[ptr-6]=mem[ptr-1];mem[ptr-2]-=1;mem[ptr-1]=1;
}
mem[ptr]+=mem[ptr-1];mem[ptr-1]=mem[ptr-6];mem[ptr-6]=1;mem[ptr]=0;mem[ptr-6]+=mem[ptr-2]*-1;mem[ptr-2]=1;ptr-=6;
while(mem[ptr]){
mem[ptr]--;mem[ptr+4]-=1;ptr+=9;
while(mem[ptr]){
mem[ptr]+=mem[ptr+2]*-1;mem[ptr+2]=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr+2]-=1;mem[ptr]+=mem[ptr+3];mem[ptr+3]=0;
while(mem[ptr]){
mem[ptr]--;mem[ptr+3]+=1;ptr-=9;
while(mem[ptr]){
ptr-=9;
}
mem[ptr+3]=1;ptr+=9;
while(mem[ptr]){
ptr+=9;
}
mem[ptr+1]+=1;
}

}
mem[ptr]++;mem[ptr]+=mem[ptr+3]*-1;mem[ptr+3]=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr+3]-=1;mem[ptr]+=mem[ptr+2];mem[ptr+2]=0;
while(mem[ptr]){
mem[ptr]--;mem[ptr+2]+=1;ptr-=9;
while(mem[ptr]){
ptr-=9;
}
mem[ptr+4]=1;ptr+=9;
while(mem[ptr]){
ptr+=9;
}
mem[ptr+1]=1;
}

}
mem[ptr]++;ptr++;
while(mem[ptr]){
mem[ptr]--;ptr--;
while(mem[ptr]){
ptr+=9;
}
ptr-=8;
}
ptr+=8;
}
ptr-=9;
while(mem[ptr]){
ptr-=9;
}
mem[ptr]+=mem[ptr+4];mem[ptr+4]=0;
while(mem[ptr]){
mem[ptr]--;mem[ptr+4]+=1;ptr+=9;
while(mem[ptr]){
mem[ptr+1]+=1;mem[ptr+1]+=mem[ptr+3]*-1;mem[ptr+3]=mem[ptr+1];mem[ptr+1]=0;ptr+=9;
}
mem[ptr-8]+=1;ptr-=9;
while(mem[ptr]){
ptr++;
while(mem[ptr]){
mem[ptr]--;mem[ptr+5]+=1;ptr++;
while(mem[ptr]){
mem[ptr]--;mem[ptr-10]+=1;mem[ptr+4]-=1;mem[ptr+4]+=mem[ptr+1];mem[ptr+1]=0;
}
mem[ptr-10]+=mem[ptr+1];mem[ptr+4]+=mem[ptr+1]*-1;mem[ptr+1]=0;ptr--;
}
ptr++;
while(mem[ptr]){
mem[ptr]--;mem[ptr+4]+=1;mem[ptr-10]+=mem[ptr+1];mem[ptr+4]+=mem[ptr+1]*-1;mem[ptr+1]=0;
}
mem[ptr+4]+=mem[ptr+1];mem[ptr+1]=0;ptr-=11;
}
mem[ptr+4]=0;
}
mem[ptr]+=mem[ptr+3];mem[ptr+3]=0;
while(mem[ptr]){
mem[ptr]--;mem[ptr+3]+=1;ptr+=9;
while(mem[ptr]){
mem[ptr+1]+=1;mem[ptr+1]+=mem[ptr+2]*-1;mem[ptr+2]=mem[ptr+1];mem[ptr+1]=0;ptr+=9;
}
mem[ptr-8]+=1;ptr-=9;
while(mem[ptr]){
ptr++;
while(mem[ptr]){
mem[ptr]--;mem[ptr+5]+=1;ptr+=2;
while(mem[ptr]){
mem[ptr]--;mem[ptr-11]+=1;mem[ptr+3]-=1;mem[ptr+3]+=mem[ptr-1];mem[ptr-1]=0;
}
mem[ptr-11]+=mem[ptr-1];mem[ptr+3]+=mem[ptr-1]*-1;mem[ptr-1]=0;ptr-=2;
}
ptr+=2;
while(mem[ptr]){
mem[ptr]--;mem[ptr+3]+=1;mem[ptr-11]+=mem[ptr-1];mem[ptr+3]+=mem[ptr-1]*-1;mem[ptr-1]=0;
}
mem[ptr+3]+=mem[ptr-1];mem[ptr-1]=0;ptr-=12;
}
mem[ptr+6]+=1;
}

}
mem[ptr]+=mem[ptr+4];mem[ptr+4]=0;
while(mem[ptr]){
mem[ptr]--;mem[ptr+4]+=1;ptr+=9;
while(mem[ptr]){
ptr+=9;
}
ptr-=9;
while(mem[ptr]){
ptr++;
while(mem[ptr]){
mem[ptr]--;mem[ptr+5]+=1;ptr++;
while(mem[ptr]){
mem[ptr]--;mem[ptr-10]+=1;mem[ptr+4]-=1;mem[ptr+4]+=mem[ptr+1];mem[ptr+1]=0;
}
mem[ptr-10]+=mem[ptr+1];mem[ptr+4]+=mem[ptr+1]*-1;mem[ptr+1]=0;ptr--;
}
ptr++;
while(mem[ptr]){
mem[ptr]--;mem[ptr+4]+=1;mem[ptr-10]+=mem[ptr+1];mem[ptr+4]+=mem[ptr+1]*-1;mem[ptr+1]=0;
}
mem[ptr+4]+=mem[ptr+1];mem[ptr+1]=0;ptr-=11;
}

}
mem[ptr+1]=0;memcpy(&mem[ptr+3],(char[2]){0,0},2);ptr+=9;
while(mem[ptr]){
memcpy(&mem[ptr+2],(char[2]){0,0},2);ptr+=9;
}
ptr-=9;
while(mem[ptr]){
ptr-=9;
}
ptr+=9;
while(mem[ptr]){
mem[ptr+1]+=mem[ptr+5];mem[ptr+5]=0;mem[ptr+2]+=mem[ptr+1];mem[ptr+5]+=mem[ptr+1];mem[ptr+1]=0;ptr+=9;
}
ptr-=9;
while(mem[ptr]){
ptr-=9;
}
mem[ptr+9]+=15;ptr+=9;
while(mem[ptr]){

while(mem[ptr]){
ptr+=9;
}
mem[ptr]++;memcpy(&mem[ptr+1],(char[9]){0,0,0,0,0,0,0,0,0},9);
while(mem[ptr]){
ptr-=9;
}
mem[ptr+9]-=1;ptr+=9;
}
mem[ptr]++;
while(mem[ptr]){
mem[ptr+1]+=1;ptr+=9;
}
ptr-=9;
while(mem[ptr]){
ptr-=9;
}
ptr+=9;
while(mem[ptr]){
mem[ptr+1]-=1;mem[ptr+1]+=mem[ptr+5];mem[ptr+5]=0;ptr++;
while(mem[ptr]){
mem[ptr]--;mem[ptr+4]+=1;ptr--;
while(mem[ptr]){
mem[ptr]--;mem[ptr]+=mem[ptr+2];mem[ptr+2]=mem[ptr];mem[ptr+3]+=mem[ptr];mem[ptr]=0;mem[ptr]++;ptr+=9;
}
ptr-=8;
while(mem[ptr]){
ptr-=9;
}

}
ptr+=9;
while(mem[ptr]){
ptr+=9;
}
ptr-=9;
while(mem[ptr]){
mem[ptr+10]+=mem[ptr+1];mem[ptr+1]=0;ptr-=9;
}
mem[ptr+10]+=mem[ptr+1];mem[ptr+1]=0;mem[ptr]++;ptr+=8;
}
ptr-=9;
while(mem[ptr]){
mem[ptr+1]=0;mem[ptr]--;ptr+=3;
while(mem[ptr]){
mem[ptr]--;mem[ptr-3]+=1;mem[ptr-9]+=mem[ptr-2];mem[ptr-3]+=mem[ptr-2]*-1;mem[ptr-2]=mem[ptr-3];mem[ptr-3]=0;
}
mem[ptr]+=mem[ptr-2];mem[ptr-3]+=1;mem[ptr-2]=0;ptr-=12;
}
ptr+=9;
while(mem[ptr]){
mem[ptr-33]+=mem[ptr+3];mem[ptr+3]=0;ptr+=9;
}
ptr-=9;
while(mem[ptr]){
ptr-=9;
}
mem[ptr+5]=0;mem[ptr+9]+=15;ptr+=9;
while(mem[ptr]){

while(mem[ptr]){
ptr+=9;
}
mem[ptr-9]-=1;ptr-=18;
while(mem[ptr]){
ptr-=9;
}
mem[ptr+9]-=1;ptr+=9;
}
mem[ptr]++;
while(mem[ptr]){
mem[ptr]+=mem[ptr+3]*-1;mem[ptr+3]=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr+3]-=1;mem[ptr]+=mem[ptr+4];mem[ptr+4]=0;
while(mem[ptr]){
mem[ptr]--;mem[ptr+4]+=1;ptr-=9;
while(mem[ptr]){
ptr-=9;
}
mem[ptr+4]=1;ptr+=9;
while(mem[ptr]){
ptr+=9;
}
mem[ptr+1]+=1;
}

}
mem[ptr]++;mem[ptr]+=mem[ptr+4]*-1;mem[ptr+4]=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr+4]-=1;mem[ptr]+=mem[ptr+3];mem[ptr+3]=0;
while(mem[ptr]){
mem[ptr]--;mem[ptr+3]+=1;ptr-=9;
while(mem[ptr]){
ptr-=9;
}
mem[ptr+3]=1;ptr+=9;
while(mem[ptr]){
ptr+=9;
}
mem[ptr+1]=1;
}

}
mem[ptr]++;ptr++;
while(mem[ptr]){
mem[ptr]--;ptr--;
while(mem[ptr]){
ptr+=9;
}
ptr-=8;
}
ptr+=8;
}
ptr-=9;
while(mem[ptr]){
ptr-=9;
}
mem[ptr]+=mem[ptr+3];mem[ptr+3]=0;
while(mem[ptr]){
mem[ptr]--;mem[ptr+3]+=1;ptr+=9;
while(mem[ptr]){
mem[ptr+1]+=1;mem[ptr+1]+=mem[ptr+4]*-1;mem[ptr+4]=mem[ptr+1];mem[ptr+1]=0;ptr+=9;
}
mem[ptr-8]+=1;ptr-=9;
while(mem[ptr]){
ptr++;
while(mem[ptr]){
mem[ptr]--;mem[ptr+1]+=1;ptr+=2;
while(mem[ptr]){
mem[ptr]--;mem[ptr-11]+=1;mem[ptr-1]-=1;mem[ptr-1]+=mem[ptr+1];mem[ptr+1]=0;
}
mem[ptr-11]+=mem[ptr+1];mem[ptr-1]+=mem[ptr+1]*-1;mem[ptr+1]=0;ptr-=2;
}
ptr+=2;
while(mem[ptr]){
mem[ptr]--;mem[ptr-1]+=1;mem[ptr-11]+=mem[ptr+1];mem[ptr-1]+=mem[ptr+1]*-1;mem[ptr+1]=0;
}
mem[ptr-1]+=mem[ptr+1];mem[ptr+1]=0;ptr-=12;
}

}
mem[ptr]+=mem[ptr+4];mem[ptr+4]=0;
while(mem[ptr]){
mem[ptr]--;mem[ptr+4]+=1;ptr+=9;
while(mem[ptr]){
mem[ptr+1]+=1;mem[ptr+1]+=mem[ptr+3]*-1;mem[ptr+3]=mem[ptr+1];mem[ptr+1]=0;ptr+=9;
}
mem[ptr-8]+=1;ptr-=9;
while(mem[ptr]){
ptr++;
while(mem[ptr]){
mem[ptr]--;mem[ptr+1]+=1;ptr+=3;
while(mem[ptr]){
mem[ptr]--;mem[ptr-12]+=1;mem[ptr-2]-=1;mem[ptr-2]+=mem[ptr-1];mem[ptr-1]=0;
}
mem[ptr-12]+=mem[ptr-1];mem[ptr-2]+=mem[ptr-1]*-1;mem[ptr-1]=0;ptr-=3;
}
ptr+=3;
while(mem[ptr]){
mem[ptr]--;mem[ptr-2]+=1;mem[ptr-12]+=mem[ptr-1];mem[ptr-2]+=mem[ptr-1]*-1;mem[ptr-1]=0;
}
mem[ptr-2]+=mem[ptr-1];mem[ptr-1]=0;ptr-=13;
}
mem[ptr+5]+=1;
}
ptr+=9;
while(mem[ptr]){
memcpy(&mem[ptr+3],(char[3]){0,0,0},3);ptr+=9;
}
ptr-=9;
while(mem[ptr]){
ptr-=9;
}
memcpy(&mem[ptr+3],(char[2]){0,0},2);ptr+=9;
while(mem[ptr]){
mem[ptr+1]+=mem[ptr+7];mem[ptr+7]=0;mem[ptr+3]+=mem[ptr+1];mem[ptr+7]+=mem[ptr+1];mem[ptr+1]=0;ptr+=9;
}
ptr-=9;
while(mem[ptr]){
ptr-=9;
}
mem[ptr+4]+=1;mem[ptr]+=mem[ptr+5];mem[ptr+4]+=mem[ptr+5]*-1;mem[ptr+5]=0;ptr+=7;
while(mem[ptr]){
mem[ptr]--;mem[ptr-3]+=mem[ptr-7]*2;mem[ptr-2]+=mem[ptr-7];mem[ptr-7]=mem[ptr-2];mem[ptr-3]-=1;mem[ptr-2]=1;
}
mem[ptr]+=mem[ptr-2];mem[ptr-2]=mem[ptr-7];mem[ptr-7]=1;mem[ptr-7]+=mem[ptr-3]*-1;mem[ptr-3]=1;ptr-=7;
while(mem[ptr]){
mem[ptr]--;mem[ptr+4]-=1;ptr+=9;
while(mem[ptr]){
mem[ptr]+=mem[ptr+3]*-1;mem[ptr+3]=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr+3]-=1;mem[ptr]+=mem[ptr+2];mem[ptr+2]=0;
while(mem[ptr]){
mem[ptr]--;mem[ptr+2]+=1;ptr-=9;
while(mem[ptr]){
ptr-=9;
}
mem[ptr+4]=1;ptr+=9;
while(mem[ptr]){
ptr+=9;
}
mem[ptr+1]+=1;
}

}
mem[ptr]++;mem[ptr]+=mem[ptr+2]*-1;mem[ptr+2]=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr+2]-=1;mem[ptr]+=mem[ptr+3];mem[ptr+3]=0;
while(mem[ptr]){
mem[ptr]--;mem[ptr+3]+=1;ptr-=9;
while(mem[ptr]){
ptr-=9;
}
mem[ptr+3]=1;ptr+=9;
while(mem[ptr]){
ptr+=9;
}
mem[ptr+1]=1;
}

}
mem[ptr]++;ptr++;
while(mem[ptr]){
mem[ptr]--;ptr--;
while(mem[ptr]){
ptr+=9;
}
ptr-=8;
}
ptr+=8;
}
ptr-=9;
while(mem[ptr]){
ptr-=9;
}
mem[ptr]+=mem[ptr+3];mem[ptr+3]=0;
while(mem[ptr]){
mem[ptr]--;mem[ptr+3]+=1;ptr+=9;
while(mem[ptr]){
mem[ptr+1]+=1;mem[ptr+1]+=mem[ptr+2]*-1;mem[ptr+2]=mem[ptr+1];mem[ptr+1]=0;ptr+=9;
}
mem[ptr-8]+=1;ptr-=9;
while(mem[ptr]){
ptr++;
while(mem[ptr]){
mem[ptr]--;mem[ptr+4]+=1;ptr+=2;
while(mem[ptr]){
mem[ptr]--;mem[ptr-11]+=1;mem[ptr+2]-=1;mem[ptr+2]+=mem[ptr-1];mem[ptr-1]=0;
}
mem[ptr-11]+=mem[ptr-1];mem[ptr+2]+=mem[ptr-1]*-1;mem[ptr-1]=0;ptr-=2;
}
ptr+=2;
while(mem[ptr]){
mem[ptr]--;mem[ptr+2]+=1;mem[ptr-11]+=mem[ptr-1];mem[ptr+2]+=mem[ptr-1]*-1;mem[ptr-1]=0;
}
mem[ptr+2]+=mem[ptr-1];mem[ptr-1]=0;ptr-=12;
}
mem[ptr+5]=0;mem[ptr]+=mem[ptr+7];mem[ptr+7]=0;mem[ptr+5]+=mem[ptr];mem[ptr+7]+=mem[ptr];mem[ptr]=0;
}
mem[ptr]+=mem[ptr+4];mem[ptr+4]=0;
while(mem[ptr]){
mem[ptr]--;mem[ptr+4]+=1;ptr+=9;
while(mem[ptr]){
mem[ptr+1]+=1;mem[ptr+1]+=mem[ptr+3]*-1;mem[ptr+3]=mem[ptr+1];mem[ptr+1]=0;ptr+=9;
}
mem[ptr-8]+=1;ptr-=9;
while(mem[ptr]){
ptr++;
while(mem[ptr]){
mem[ptr]--;mem[ptr+4]+=1;ptr++;
while(mem[ptr]){
mem[ptr]--;mem[ptr-10]+=1;mem[ptr+3]-=1;mem[ptr+3]+=mem[ptr+1];mem[ptr+1]=0;
}
mem[ptr-10]+=mem[ptr+1];mem[ptr+3]+=mem[ptr+1]*-1;mem[ptr+1]=0;ptr--;
}
ptr++;
while(mem[ptr]){
mem[ptr]--;mem[ptr+3]+=1;mem[ptr-10]+=mem[ptr+1];mem[ptr+3]+=mem[ptr+1]*-1;mem[ptr+1]=0;
}
mem[ptr+3]+=mem[ptr+1];mem[ptr+1]=0;ptr-=11;
}

}
mem[ptr+4]=0;
}
mem[ptr]+=mem[ptr+4];mem[ptr+4]=0;
while(mem[ptr]){
mem[ptr]--;mem[ptr+4]+=1;mem[ptr+5]=0;mem[ptr]+=mem[ptr+7];mem[ptr+7]=0;mem[ptr+5]+=mem[ptr];mem[ptr+7]+=mem[ptr];mem[ptr]=0;ptr+=9;
while(mem[ptr]){
ptr+=9;
}
ptr-=9;
while(mem[ptr]){
ptr++;
while(mem[ptr]){
mem[ptr]--;mem[ptr+4]+=1;ptr++;
while(mem[ptr]){
mem[ptr]--;mem[ptr-10]+=1;mem[ptr+3]-=1;mem[ptr+3]+=mem[ptr+1];mem[ptr+1]=0;
}
mem[ptr-10]+=mem[ptr+1];mem[ptr+3]+=mem[ptr+1]*-1;mem[ptr+1]=0;ptr--;
}
ptr++;
while(mem[ptr]){
mem[ptr]--;mem[ptr+3]+=1;mem[ptr-10]+=mem[ptr+1];mem[ptr+3]+=mem[ptr+1]*-1;mem[ptr+1]=0;
}
mem[ptr+3]+=mem[ptr+1];mem[ptr+1]=0;ptr-=11;
}

}
ptr+=9;
while(mem[ptr]){
memcpy(&mem[ptr+2],(char[2]){0,0},2);ptr+=9;
}
ptr-=9;
while(mem[ptr]){
ptr-=9;
}
memcpy(&mem[ptr+3],(char[2]){0,0},2);ptr+=9;
while(mem[ptr]){
mem[ptr+1]+=mem[ptr+5];mem[ptr+5]=0;mem[ptr+2]+=mem[ptr+1];mem[ptr+5]+=mem[ptr+1];mem[ptr+1]=0;ptr+=9;
}
ptr-=9;
while(mem[ptr]){
ptr-=9;
}
ptr+=9;
while(mem[ptr]){
mem[ptr+1]+=mem[ptr+6];mem[ptr+6]=0;mem[ptr+3]+=mem[ptr+1];mem[ptr+6]+=mem[ptr+1];mem[ptr+1]=0;ptr+=9;
}
ptr-=9;
while(mem[ptr]){
ptr-=9;
}
mem[ptr+9]+=15;ptr+=9;
while(mem[ptr]){

while(mem[ptr]){
ptr+=9;
}
mem[ptr]++;memcpy(&mem[ptr+1],(char[9]){0,0,0,0,0,0,0,0,0},9);
while(mem[ptr]){
ptr-=9;
}
mem[ptr+9]-=1;ptr+=9;
}
mem[ptr]++;
while(mem[ptr]){
mem[ptr+1]+=1;ptr+=9;
}
ptr-=9;
while(mem[ptr]){
ptr-=9;
}
ptr+=9;
while(mem[ptr]){
mem[ptr+1]-=1;mem[ptr+1]+=mem[ptr+5];mem[ptr+5]=0;ptr++;
while(mem[ptr]){
mem[ptr]--;mem[ptr+4]+=1;ptr--;
while(mem[ptr]){
mem[ptr]--;mem[ptr]+=mem[ptr+2];mem[ptr+2]=mem[ptr];mem[ptr+4]+=mem[ptr];mem[ptr]=0;mem[ptr]++;ptr+=9;
}
ptr-=8;
while(mem[ptr]){
ptr-=9;
}

}
ptr+=9;
while(mem[ptr]){
ptr+=9;
}
ptr-=9;
while(mem[ptr]){
mem[ptr+10]+=mem[ptr+1];mem[ptr+1]=0;ptr-=9;
}
mem[ptr+10]+=mem[ptr+1];mem[ptr+1]=0;mem[ptr]++;ptr+=8;
}
ptr-=9;
while(mem[ptr]){
mem[ptr+1]=0;mem[ptr]--;ptr+=4;
while(mem[ptr]){
mem[ptr]--;mem[ptr-4]+=1;mem[ptr-9]+=mem[ptr-3];mem[ptr-4]+=mem[ptr-3]*-1;mem[ptr-3]=mem[ptr-4];mem[ptr-4]=0;
}
mem[ptr]+=mem[ptr-3];mem[ptr-4]+=1;mem[ptr-3]=0;ptr-=13;
}
ptr+=9;
while(mem[ptr]){
mem[ptr+1]+=1;ptr+=9;
}
ptr-=9;
while(mem[ptr]){
ptr-=9;
}
ptr+=9;
while(mem[ptr]){
mem[ptr+1]-=1;mem[ptr+1]+=mem[ptr+6];mem[ptr+6]=0;ptr++;
while(mem[ptr]){
mem[ptr]--;mem[ptr+5]+=1;ptr--;
while(mem[ptr]){
mem[ptr]--;mem[ptr]+=mem[ptr+3];mem[ptr+3]=mem[ptr];mem[ptr+4]+=mem[ptr];mem[ptr]=0;mem[ptr]++;ptr+=9;
}
ptr-=8;
while(mem[ptr]){
ptr-=9;
}

}
ptr+=9;
while(mem[ptr]){
ptr+=9;
}
ptr-=9;
while(mem[ptr]){
mem[ptr+11]+=mem[ptr+2];mem[ptr+2]=0;ptr-=9;
}
mem[ptr+11]+=mem[ptr+2];mem[ptr+2]=0;mem[ptr]++;ptr+=8;
}
ptr-=9;
while(mem[ptr]){
mem[ptr+1]=0;mem[ptr]--;ptr+=4;
while(mem[ptr]){
mem[ptr]--;mem[ptr-4]+=1;mem[ptr-9]+=mem[ptr-3];mem[ptr-4]+=mem[ptr-3]*-1;mem[ptr-3]=mem[ptr-4];mem[ptr-4]=0;
}
mem[ptr]+=mem[ptr-3];mem[ptr-4]+=1;mem[ptr-3]=0;ptr-=13;
}
ptr+=9;
while(mem[ptr]){
mem[ptr-32]+=mem[ptr+4];mem[ptr+4]=0;ptr+=9;
}
ptr-=9;
while(mem[ptr]){
ptr-=9;
}
mem[ptr+9]+=15;ptr+=9;
while(mem[ptr]){

while(mem[ptr]){
ptr+=9;
}
mem[ptr-9]-=1;ptr-=18;
while(mem[ptr]){
ptr-=9;
}
mem[ptr+9]-=1;ptr+=9;
}
mem[ptr]++;mem[ptr+21]+=1;ptr+=18;
while(mem[ptr]){
ptr-=9;
}
ptr+=9;
while(mem[ptr]){
mem[ptr]+=mem[ptr+3]*-1;mem[ptr+3]=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr+3]-=1;mem[ptr]+=mem[ptr+4];mem[ptr+4]=0;
while(mem[ptr]){
mem[ptr]--;mem[ptr+4]+=1;ptr-=9;
while(mem[ptr]){
ptr-=9;
}
mem[ptr+4]=1;ptr+=9;
while(mem[ptr]){
ptr+=9;
}
mem[ptr+1]+=1;
}

}
mem[ptr]++;mem[ptr]+=mem[ptr+4]*-1;mem[ptr+4]=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr+4]-=1;mem[ptr]+=mem[ptr+3];mem[ptr+3]=0;
while(mem[ptr]){
mem[ptr]--;mem[ptr+3]+=1;ptr-=9;
while(mem[ptr]){
ptr-=9;
}
mem[ptr+3]=1;ptr+=9;
while(mem[ptr]){
ptr+=9;
}
mem[ptr+1]=1;
}

}
mem[ptr]++;ptr++;
while(mem[ptr]){
mem[ptr]--;ptr--;
while(mem[ptr]){
ptr+=9;
}
ptr-=8;
}
ptr+=8;
}
ptr-=9;
while(mem[ptr]){
ptr-=9;
}
mem[ptr+2]-=1;mem[ptr]+=mem[ptr+4];mem[ptr+4]=0;
while(mem[ptr]){
mem[ptr]--;mem[ptr+2]=0;mem[ptr+4]+=1;
}
ptr+=2;
}
mem[ptr-2]+=1;mem[ptr-2]+=mem[ptr+2]*-1;mem[ptr+2]=1;ptr-=2;
while(mem[ptr]){
mem[ptr]--;mem[ptr+4]-=1;putchar(mem[ptr-2]);
}
ptr+=4;
while(mem[ptr]){
mem[ptr]--;putchar(mem[ptr-7]);
}
memcpy(&mem[ptr-3],(char[6]){0,0,0,0,0,0},6);ptr+=5;
while(mem[ptr]){
memcpy(&mem[ptr+1],(char[6]){0,0,0,0,0,0},6);ptr+=9;
}
ptr-=9;
while(mem[ptr]){
ptr-=9;
}
ptr+=9;
while(mem[ptr]){
mem[ptr+5]=0;ptr+=9;
}
ptr-=9;
while(mem[ptr]){
ptr-=9;
}
mem[ptr+1]+=11;ptr++;
while(mem[ptr]){
mem[ptr]--;mem[ptr+9]+=mem[ptr];mem[ptr]=0;ptr+=9;
}
mem[ptr+4]+=1;mem[ptr+13]+=1;ptr--;
while(mem[ptr]){
ptr-=9;
}
mem[ptr]+=mem[ptr+7];mem[ptr+7]=0;
while(mem[ptr]){
mem[ptr]--;mem[ptr+7]=0;ptr+=9;
while(mem[ptr]){
ptr+=9;
}
ptr-=9;
while(mem[ptr]){
mem[ptr+1]+=mem[ptr+7];mem[ptr+7]=0;ptr++;
while(mem[ptr]){
mem[ptr]--;mem[ptr+6]+=1;ptr--;
while(mem[ptr]){
ptr-=9;
}
mem[ptr+7]=1;ptr+=10;
}
ptr-=10;
}

}
mem[ptr]+=mem[ptr+7];mem[ptr+7]=0;
while(mem[ptr]){
mem[ptr]--;mem[ptr+7]+=1;ptr+=9;
while(mem[ptr]){
mem[ptr+1]+=1;mem[ptr+1]+=mem[ptr+5]*-1;mem[ptr+5]=mem[ptr+1];mem[ptr+1]=0;ptr+=9;
}
mem[ptr-2]+=1;ptr-=9;
while(mem[ptr]){
mem[ptr+7]+=mem[ptr+5];mem[ptr+5]=0;ptr-=9;
}
ptr+=9;
while(mem[ptr]){
ptr+=9;
}
ptr-=9;
while(mem[ptr]){
mem[ptr+1]=0;mem[ptr]--;ptr+=7;
while(mem[ptr]){
mem[ptr]--;mem[ptr-7]+=1;mem[ptr-9]+=mem[ptr-6];mem[ptr-7]+=mem[ptr-6]*-1;mem[ptr-6]=mem[ptr-7];mem[ptr-7]=0;
}
mem[ptr]+=mem[ptr-6];mem[ptr-7]+=1;mem[ptr-6]=0;ptr-=16;
}
mem[ptr+3]=1;mem[ptr+7]-=1;
}
mem[ptr]++;mem[ptr]+=mem[ptr+7]*-1;mem[ptr+7]=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr+7]-=1;ptr+=9;
while(mem[ptr]){
mem[ptr+7]+=mem[ptr+5];mem[ptr+5]=0;ptr+=9;
}
ptr-=9;
while(mem[ptr]){
mem[ptr+1]=0;mem[ptr]--;ptr+=7;
while(mem[ptr]){
mem[ptr]--;mem[ptr-7]+=1;mem[ptr-9]+=mem[ptr-6];mem[ptr-7]+=mem[ptr-6]*-1;mem[ptr-6]=mem[ptr-7];mem[ptr-7]=0;
}
mem[ptr]+=mem[ptr-6];mem[ptr-7]+=1;mem[ptr-6]=0;ptr-=16;
}
mem[ptr+1]+=5;ptr++;
while(mem[ptr]){
mem[ptr]--;mem[ptr+9]+=mem[ptr];mem[ptr]=0;ptr+=9;
}
mem[ptr+4]+=1;ptr--;
while(mem[ptr]){
ptr-=9;
}
ptr+=9;
while(mem[ptr]){
mem[ptr]+=mem[ptr+5]*-1;mem[ptr+5]=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr+5]-=1;mem[ptr]+=mem[ptr+7];mem[ptr+7]=0;
while(mem[ptr]){
mem[ptr]--;mem[ptr+7]+=1;ptr-=9;
while(mem[ptr]){
ptr-=9;
}
mem[ptr+4]=1;ptr+=9;
while(mem[ptr]){
ptr+=9;
}
mem[ptr+1]+=1;
}

}
mem[ptr]++;mem[ptr]+=mem[ptr+7]*-1;mem[ptr+7]=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr+7]-=1;mem[ptr]+=mem[ptr+5];mem[ptr+5]=0;
while(mem[ptr]){
mem[ptr]--;mem[ptr+5]+=1;ptr-=9;
while(mem[ptr]){
ptr-=9;
}
mem[ptr+3]=1;ptr+=9;
while(mem[ptr]){
ptr+=9;
}
mem[ptr+1]=1;
}

}
mem[ptr]++;ptr++;
while(mem[ptr]){
mem[ptr]--;ptr--;
while(mem[ptr]){
ptr+=9;
}
ptr-=8;
}
ptr+=8;
}
ptr-=9;
while(mem[ptr]){
ptr-=9;
}
mem[ptr+1]+=5;mem[ptr+4]=0;ptr++;
while(mem[ptr]){
mem[ptr]--;mem[ptr+9]+=mem[ptr];mem[ptr]=0;ptr+=9;
}
mem[ptr+4]-=1;ptr--;
while(mem[ptr]){
ptr-=9;
}

}
ptr+=3;
}
putchar(mem[ptr-4]);ptr+=6;
while(mem[ptr]){
mem[ptr+6]=0;ptr+=9;
}
ptr-=9;
while(mem[ptr]){
ptr-=9;
}
mem[ptr+1]+=10;ptr++;
while(mem[ptr]){
mem[ptr]--;mem[ptr+9]+=mem[ptr];mem[ptr]=0;ptr+=9;
}
mem[ptr+5]+=1;mem[ptr+14]+=1;ptr--;
while(mem[ptr]){
ptr-=9;
}
mem[ptr]+=mem[ptr+8];mem[ptr+8]=0;
while(mem[ptr]){
mem[ptr]--;mem[ptr+8]=0;ptr+=9;
while(mem[ptr]){
ptr+=9;
}
ptr-=9;
while(mem[ptr]){
mem[ptr+1]+=mem[ptr+8];mem[ptr+8]=0;ptr++;
while(mem[ptr]){
mem[ptr]--;mem[ptr+7]+=1;ptr--;
while(mem[ptr]){
ptr-=9;
}
mem[ptr+8]=1;ptr+=10;
}
ptr-=10;
}

}
mem[ptr]+=mem[ptr+8];mem[ptr+8]=0;
while(mem[ptr]){
mem[ptr]--;mem[ptr+8]+=1;ptr+=9;
while(mem[ptr]){
mem[ptr+1]+=1;mem[ptr+1]+=mem[ptr+6]*-1;mem[ptr+6]=mem[ptr+1];mem[ptr+1]=0;ptr+=9;
}
mem[ptr-1]+=1;ptr-=9;
while(mem[ptr]){
mem[ptr+8]+=mem[ptr+6];mem[ptr+6]=0;ptr-=9;
}
ptr+=9;
while(mem[ptr]){
ptr+=9;
}
ptr-=9;
while(mem[ptr]){
mem[ptr+1]=0;mem[ptr]--;ptr+=8;
while(mem[ptr]){
mem[ptr]--;mem[ptr-8]+=1;mem[ptr-9]+=mem[ptr-7];mem[ptr-8]+=mem[ptr-7]*-1;mem[ptr-7]=mem[ptr-8];mem[ptr-8]=0;
}
mem[ptr]+=mem[ptr-7];mem[ptr-8]+=1;mem[ptr-7]=0;ptr-=17;
}
mem[ptr+3]=1;mem[ptr+8]-=1;
}
mem[ptr]++;mem[ptr]+=mem[ptr+8]*-1;mem[ptr+8]=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr+8]-=1;ptr+=9;
while(mem[ptr]){
mem[ptr+8]+=mem[ptr+6];mem[ptr+6]=0;ptr+=9;
}
ptr-=9;
while(mem[ptr]){
mem[ptr+1]=0;mem[ptr]--;ptr+=8;
while(mem[ptr]){
mem[ptr]--;mem[ptr-8]+=1;mem[ptr-9]+=mem[ptr-7];mem[ptr-8]+=mem[ptr-7]*-1;mem[ptr-7]=mem[ptr-8];mem[ptr-8]=0;
}
mem[ptr]+=mem[ptr-7];mem[ptr-8]+=1;mem[ptr-7]=0;ptr-=17;
}
mem[ptr+1]+=5;ptr++;
while(mem[ptr]){
mem[ptr]--;mem[ptr+9]+=mem[ptr];mem[ptr]=0;ptr+=9;
}
mem[ptr+5]+=1;mem[ptr+32]+=1;ptr+=26;
while(mem[ptr]){
ptr-=9;
}
ptr+=9;
while(mem[ptr]){
mem[ptr]+=mem[ptr+6]*-1;mem[ptr+6]=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr+6]-=1;mem[ptr]+=mem[ptr+8];mem[ptr+8]=0;
while(mem[ptr]){
mem[ptr]--;mem[ptr+8]+=1;ptr-=9;
while(mem[ptr]){
ptr-=9;
}
mem[ptr+4]=1;ptr+=9;
while(mem[ptr]){
ptr+=9;
}
mem[ptr+1]+=1;
}

}
mem[ptr]++;mem[ptr]+=mem[ptr+8]*-1;mem[ptr+8]=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr+8]-=1;mem[ptr]+=mem[ptr+6];mem[ptr+6]=0;
while(mem[ptr]){
mem[ptr]--;mem[ptr+6]+=1;ptr-=9;
while(mem[ptr]){
ptr-=9;
}
mem[ptr+3]=1;ptr+=9;
while(mem[ptr]){
ptr+=9;
}
mem[ptr+1]=1;
}

}
mem[ptr]++;ptr++;
while(mem[ptr]){
mem[ptr]--;ptr--;
while(mem[ptr]){
ptr+=9;
}
ptr-=8;
}
ptr+=8;
}
ptr-=9;
while(mem[ptr]){
ptr-=9;
}
mem[ptr+1]+=5;mem[ptr+4]=0;ptr++;
while(mem[ptr]){
mem[ptr]--;mem[ptr+9]+=mem[ptr];mem[ptr]=0;ptr+=9;
}
mem[ptr+5]-=1;mem[ptr+32]-=1;ptr+=26;
while(mem[ptr]){
ptr-=9;
}

}
ptr+=3;
}


  return 0;
}
