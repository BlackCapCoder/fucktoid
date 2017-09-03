#include <stdio.h>
#include <string.h>
#include <stdio.h>

int main () {
  #define TAPE_SIZE 65536
  char mem[TAPE_SIZE] = {0};
  int ptr;

memcpy(&mem[ptr],(char[10]){0,0,0,0,0,0,0,0,0,0},10);mem[ptr+21]+=1;mem[ptr+30]-=1;ptr+=21;
while(mem[ptr]){
mem[ptr]--;
while(mem[ptr]){
ptr+=10;
}
ptr-=10;
while(mem[ptr]){
mem[ptr+10]+=mem[ptr];mem[ptr]=0;ptr-=10;
}
mem[ptr+10]=getchar();mem[ptr+10]-=10;ptr+=10;
}
ptr+=10;
while(mem[ptr]){
mem[ptr]-=37;mem[ptr+9]-=1;ptr+=10;
}
ptr--;
while(mem[ptr]){
mem[ptr]++;ptr++;
while(mem[ptr]){
mem[ptr+9]+=1;ptr+=10;
}
mem[ptr-1]-=1;ptr-=11;
}
mem[ptr]--;
while(mem[ptr]){
mem[ptr+1]+=48;putchar(mem[ptr+1]);mem[ptr+1]-=48;ptr-=10;
}
mem[ptr]+=58;putchar(mem[ptr]);mem[ptr]-=26;putchar(mem[ptr]);mem[ptr]=0;mem[ptr+8]+=1;mem[ptr+12]+=2;ptr+=8;
while(mem[ptr]){
mem[ptr]=0;ptr+=2;
while(mem[ptr]){
memcpy(&mem[ptr+4],(char[6]){0,0,0,0,0,0},6);mem[ptr+5]+=mem[ptr+2];mem[ptr+6]+=mem[ptr+2];mem[ptr+2]=0;ptr+=10;
}
ptr-=10;
while(mem[ptr]){
mem[ptr+2]+=mem[ptr+6];mem[ptr+6]=0;ptr-=10;
}
ptr+=10;
while(mem[ptr]){
mem[ptr+4]+=mem[ptr+1];mem[ptr+6]+=mem[ptr+1];mem[ptr+1]=0;ptr+=10;
}
ptr-=10;
while(mem[ptr]){
mem[ptr+1]+=mem[ptr+6];mem[ptr+6]=0;ptr-=10;
}
ptr+=10;
while(mem[ptr]){
mem[ptr+3]=0;memcpy(&mem[ptr+6],(char[2]){0,0},2);ptr+=10;
}
ptr-=10;
while(mem[ptr]){
ptr-=10;
}
memcpy(&mem[ptr+8],(char[2]){1,0},2);mem[ptr+16]+=1;ptr+=8;
while(mem[ptr]){
mem[ptr]--;ptr+=2;
while(mem[ptr]){
mem[ptr+7]+=mem[ptr+6]*2;mem[ptr+6]=0;ptr+=10;
}
ptr-=10;
while(mem[ptr]){
memcpy(&mem[ptr+8],(char[2]){0,0},2);mem[ptr+8]+=mem[ptr+5]*2;mem[ptr+5]=0;ptr-=10;
}
ptr+=10;
while(mem[ptr]){
ptr+=8;
while(mem[ptr]){
mem[ptr]--;mem[ptr+1]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr+1]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr+1]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr+1]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr+1]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr+1]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr+1]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr+1]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr+1]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr+1]-=9;mem[ptr+10]+=1;mem[ptr+1]+=mem[ptr];mem[ptr]=0;
}

}

}

}

}

}

}

}

}

}
ptr+=2;
}
ptr-=10;
while(mem[ptr]){
mem[ptr+5]+=mem[ptr+9];mem[ptr+8]+=mem[ptr+9];mem[ptr+9]=0;ptr-=10;
}
ptr+=10;
while(mem[ptr]){
ptr+=7;
while(mem[ptr]){
mem[ptr]--;mem[ptr-1]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr-1]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr-1]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr-1]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr-1]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr-1]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr-1]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr-1]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr-1]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr-1]-=9;mem[ptr+10]+=1;mem[ptr-1]+=mem[ptr];mem[ptr]=0;
}

}

}

}

}

}

}

}

}

}
ptr+=3;
}
ptr-=10;
while(mem[ptr]){
mem[ptr+7]+=mem[ptr+4];mem[ptr+9]+=mem[ptr+4];mem[ptr+4]=0;ptr-=10;
}
ptr+=10;
while(mem[ptr]){
mem[ptr+4]+=mem[ptr+7];mem[ptr+7]=0;ptr+=10;
}
ptr-=10;
while(mem[ptr]){
mem[ptr+9]+=mem[ptr+8]*-1;mem[ptr+8]=0;ptr+=9;
while(mem[ptr]){
ptr-=9;
while(mem[ptr]){
mem[ptr-1]=mem[ptr+9];mem[ptr+9]=0;ptr-=10;
}
ptr+=19;
}
ptr-=19;
}
ptr+=9;
while(mem[ptr]){
mem[ptr]++;
while(mem[ptr]){
mem[ptr]++;
while(mem[ptr]){
mem[ptr]++;
while(mem[ptr]){
mem[ptr]++;
while(mem[ptr]){
mem[ptr]++;
while(mem[ptr]){
mem[ptr]++;
while(mem[ptr]){
mem[ptr]++;
while(mem[ptr]){
mem[ptr]++;
while(mem[ptr]){
mem[ptr]++;
while(mem[ptr]){
mem[ptr]++;
while(mem[ptr]){
mem[ptr-1]+=1;mem[ptr]=0;
}

}

}

}

}

}

}

}

}

}

}
ptr--;
}
ptr+=8;
while(mem[ptr]){
ptr-=6;
while(mem[ptr]){
memcpy(&mem[ptr+8],(char[2]){0,0},2);mem[ptr+7]+=mem[ptr+4];mem[ptr+8]+=mem[ptr+4];mem[ptr+4]=0;ptr+=10;
}
ptr-=10;
while(mem[ptr]){
mem[ptr+4]+=mem[ptr+8];mem[ptr+8]=mem[ptr+5];mem[ptr+9]+=mem[ptr+5];mem[ptr+5]=0;ptr-=10;
}
ptr+=10;
while(mem[ptr]){
mem[ptr+5]+=mem[ptr+9];mem[ptr+9]=0;ptr+=10;
}
ptr-=10;
while(mem[ptr]){
mem[ptr+7]+=mem[ptr+8]*-1;mem[ptr+8]=0;ptr-=10;
}
ptr+=10;
while(mem[ptr]){
ptr+=7;
while(mem[ptr]){
mem[ptr]--;mem[ptr+1]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr+1]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr+1]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr+1]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr+1]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr+1]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr+1]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr+1]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr+1]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr+1]+=1;
while(mem[ptr]){
mem[ptr]+=10;
while(mem[ptr]){
mem[ptr]++;mem[ptr+1]-=1;
}
mem[ptr+10]-=1;
}

}

}

}

}

}

}

}

}

}

}
ptr+=3;
}
mem[ptr+7]+=1;ptr+=7;
while(mem[ptr]){
mem[ptr]=0;ptr-=17;
while(mem[ptr]){
mem[ptr+4]=mem[ptr+8];mem[ptr+8]=mem[ptr+6];mem[ptr+6]=0;ptr-=10;
}
ptr+=10;
while(mem[ptr]){
mem[ptr+6]+=mem[ptr+8];mem[ptr+9]+=mem[ptr+8];mem[ptr+8]=0;ptr+=10;
}
ptr-=10;
while(mem[ptr]){
mem[ptr+9]+=mem[ptr+3];mem[ptr+3]=0;ptr-=10;
}
ptr+=10;
while(mem[ptr]){
ptr+=9;
while(mem[ptr]){
mem[ptr]--;mem[ptr-6]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr-6]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr-6]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr-6]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr-6]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr-6]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr-6]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr-6]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr-6]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr-6]-=9;mem[ptr+10]+=1;mem[ptr-6]+=mem[ptr];mem[ptr]=0;
}

}

}

}

}

}

}

}

}

}
ptr++;
}
ptr+=7;
}
ptr-=17;
while(mem[ptr]){
ptr-=10;
}
ptr+=10;
while(mem[ptr]){
mem[ptr+8]=0;mem[ptr+7]+=mem[ptr+6];mem[ptr+6]=0;mem[ptr+8]+=mem[ptr+5];mem[ptr+5]=0;ptr+=10;
}
ptr-=10;
while(mem[ptr]){
mem[ptr]++;ptr+=7;
while(mem[ptr]){
mem[ptr]--;mem[ptr-7]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr-7]-=1;mem[ptr-1]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr-7]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr-7]-=1;mem[ptr-1]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr-7]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr-7]-=1;mem[ptr-1]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr-7]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr-7]-=1;mem[ptr-1]+=1;mem[ptr-7]+=mem[ptr];mem[ptr]=0;
}

}

}

}

}

}

}

}
mem[ptr]+=mem[ptr-7];mem[ptr-7]=-1;ptr-=17;
}
mem[ptr-4]+=mem[ptr+7];mem[ptr+7]=0;ptr+=10;
while(mem[ptr]){
mem[ptr-4]+=mem[ptr+7]*5;mem[ptr+7]=0;ptr+=10;
}
ptr-=10;
while(mem[ptr]){
mem[ptr]++;ptr+=8;
while(mem[ptr]){
mem[ptr]--;mem[ptr-8]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr-8]-=1;mem[ptr-3]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr-8]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr-8]-=1;mem[ptr-3]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr-8]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr-8]-=1;mem[ptr-3]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr-8]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr-8]-=1;mem[ptr-3]+=1;mem[ptr-8]+=mem[ptr];mem[ptr]=0;
}

}

}

}

}

}

}

}
mem[ptr]+=mem[ptr-8];mem[ptr-8]=-1;ptr-=18;
}
mem[ptr-5]+=mem[ptr+8];mem[ptr+8]=0;ptr+=10;
while(mem[ptr]){
mem[ptr-5]+=mem[ptr+8]*5;mem[ptr+8]=0;ptr+=10;
}
ptr-=10;
while(mem[ptr]){
ptr-=10;
}
ptr+=16;
}
ptr-=6;
while(mem[ptr]){
mem[ptr+7]+=mem[ptr+3];mem[ptr+8]+=mem[ptr+3];mem[ptr+3]=0;ptr+=10;
}
ptr-=10;
while(mem[ptr]){
mem[ptr+3]+=mem[ptr+7];mem[ptr+7]=mem[ptr+2];mem[ptr+9]+=mem[ptr+2];mem[ptr+2]=0;ptr-=10;
}
ptr+=10;
while(mem[ptr]){
mem[ptr+2]+=mem[ptr+7];mem[ptr+7]=0;ptr+=10;
}
ptr-=10;
while(mem[ptr]){
mem[ptr+8]+=mem[ptr+9]*-1;mem[ptr+9]=0;ptr+=8;
while(mem[ptr]){
ptr-=8;
while(mem[ptr]){
mem[ptr-2]=mem[ptr+8];mem[ptr+8]=0;ptr-=10;
}
ptr+=18;
}
ptr-=18;
}
ptr+=8;
while(mem[ptr]){
mem[ptr+1]-=1;
while(mem[ptr]){
mem[ptr]++;
while(mem[ptr]){
mem[ptr]++;
while(mem[ptr]){
mem[ptr]++;
while(mem[ptr]){
mem[ptr]++;
while(mem[ptr]){
mem[ptr]++;
while(mem[ptr]){
mem[ptr]++;
while(mem[ptr]){
mem[ptr]++;
while(mem[ptr]){
mem[ptr]++;
while(mem[ptr]){
mem[ptr]++;
while(mem[ptr]){
mem[ptr]=0;mem[ptr+1]+=1;
}

}

}

}

}

}

}

}

}

}

}
mem[ptr+1]+=1;ptr++;
while(mem[ptr]){
memcpy(&mem[ptr-1],(char[2]){1,0},2);mem[ptr+3]+=1;ptr+=11;
while(mem[ptr]){
ptr+=10;
}
ptr-=10;
while(mem[ptr]){
ptr-=6;
while(mem[ptr]){
ptr-=4;
while(mem[ptr]){
ptr-=10;
}
mem[ptr+4]+=1;ptr-=6;
}
ptr-=4;
}
ptr+=20;
while(mem[ptr]){
ptr+=10;
}
ptr-=10;
while(mem[ptr]){
ptr-=10;
}
mem[ptr+4]-=1;ptr+=4;
while(mem[ptr]){
mem[ptr]=0;mem[ptr+8]-=1;ptr+=6;
while(mem[ptr]){
mem[ptr+1]=mem[ptr+3];mem[ptr+3]=0;ptr+=10;
}
ptr-=10;
while(mem[ptr]){
mem[ptr]++;ptr+=2;
while(mem[ptr]){
mem[ptr+8]+=1;ptr+=10;
}
mem[ptr-2]-=1;ptr-=12;
}
mem[ptr]--;
while(mem[ptr]){
mem[ptr+2]+=48;putchar(mem[ptr+2]);mem[ptr+2]-=48;ptr-=10;
}
mem[ptr]+=32;putchar(mem[ptr]);mem[ptr]=0;ptr+=4;
}
ptr+=6;
while(mem[ptr]){
ptr+=2;
while(mem[ptr]){
mem[ptr]--;mem[ptr+5]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr+5]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr+5]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr+5]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr+5]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr+5]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr+5]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr+5]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr+5]+=1;
while(mem[ptr]){
mem[ptr]--;mem[ptr+5]-=9;mem[ptr+10]+=1;mem[ptr+5]+=mem[ptr];mem[ptr]=0;
}

}

}

}

}

}

}

}

}

}
ptr+=8;
}
ptr-=10;
while(mem[ptr]){
mem[ptr+2]+=mem[ptr+7];mem[ptr+7]=0;ptr-=10;
}
ptr+=9;
}
ptr--;
}
ptr+=2;
while(mem[ptr]){
ptr+=10;
}
ptr-=10;
while(mem[ptr]){
mem[ptr]++;ptr++;
while(mem[ptr]){
mem[ptr+9]+=1;ptr+=10;
}
mem[ptr-1]-=1;ptr-=11;
}
mem[ptr]--;
while(mem[ptr]){
mem[ptr+1]+=48;putchar(mem[ptr+1]);ptr-=10;
}
mem[ptr]+=10;putchar(mem[ptr]);

  return 0;
}
