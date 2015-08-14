#define MAXN 40// numero maximo de vertices
//#define MAXN 2*Num
#include "nauty.h"
#include "profile_time.h"
struct Canon{
    graph *C;
    struct Canon *menores;
    struct Canon *mayores;
};
int comparargraficas(graph *C1, graph *C2,int  TG){
    //regresa 0 si son igules, 1 si C1 es mayor
    //que C2 y -1 si C1 es menor que C2.
    size_t k;
    for (k = 0; k < (size_t)TG; ++k){
        if (C1[k] < C2[k])
            return 1;
        if (C1[k] > C2[k])
            return -1;
    }
    return 0;
}
int buscargrafica(struct Canon *pi, graph *C, int TG, struct Canon **padre, int *comparacion){
    //0 si la grafica es nueva, 1 si es repetido
    while (pi!=NULL) {
        *padre=pi;
        *comparacion=comparargraficas(C, pi->C, TG);
        if (*comparacion==0)
            return 1;
        if (*comparacion==1)
            pi=pi->mayores;
        else if (*comparacion==-1)
            pi=pi->menores;
    }
    return 0;
}
int Nuevo(struct Canon **raiz, graph *C, int TG){
    //Da 0 si se agrego el nodo.
    //Da 1 sino.
    struct Canon *padre;
    int comparacion;
    if(buscargrafica(*raiz, C, TG, &padre, &comparacion)==0){
        size_t k;
        struct Canon *N= (struct Canon *)malloc(sizeof(struct Canon));
        if (N==NULL)
            return 2;
        N->C=(graph *)malloc(sizeof(graph)*TG);
        N->menores=NULL;
        N->mayores=NULL;
        for (k = 0; k < (size_t)TG; ++k)
            (N)->C[k]=C[k];
        if (*raiz==NULL)
            *raiz=N;
        else{
            if (comparacion==1)
                padre->mayores=N;
            else
                padre->menores=N;
        }
        return 0;
    }
    return 1;
}
void Borrar(struct Canon *pi){
    if(pi!=NULL){
        Borrar(pi->mayores);
        Borrar(pi->menores);
        free(pi->C);
        free(pi);
    }
}
int buscarpar(int *Tripletas, int tam, int a, int b){
    int i;
    b=(1<<b);
    b|=(1<<a);
    for (i=0; i<tam; i++)
        if ((Tripletas[i]&b)==b)
            return 1;
    return 0;
}
void imprimirtripleta(int *Vertices, int NV, int TL){
    int i, j;
    printf("\n");
    for (i=0; i<NV; i++){
        printf("(");
        for (j=0; j<TL-1; j++)
            printf("%i, ", Vertices[TL*i+j]+1);
        printf("%i)", Vertices[TL*i+TL-1]+1);
    }
}
int buscarvertice(int *Tripletas, int k, int a, int TL){
    int i, contador;
    for (i=0, contador=0; i<k/(TL-1); i++)
        if (Tripletas[i]&(1<<a)){
            contador++;
            if (contador==TL)
                return 1;
        }
    return 0;
}
int verificar(int *Tripletas, int *Vertices, int k, int  a, int TL){
    int i;
    if(buscarvertice(Tripletas, k, a, TL)==1)
        return 1;
    if (k%(TL-1)==0){
        if (Vertices[(k/(TL-1))*TL-TL]==Vertices[(k/(TL-1))*TL]&&a<Vertices[(k/(TL-1))*TL-TL+1])
            return 1;
    }
    for (i=0; i<=k%(TL-1); i++) {
        if(buscarpar(Tripletas, k/(TL-1), Vertices[(k/(TL-1))*TL+i], a)==1)
            return 1;
    }
    return 0;
}
void actualizar(int *Tripletas, int *Vertices, int *S, int k, int  a, int TL){
    if (k%(TL-1)==TL-2) {
        int minimo, i;
        Tripletas[k/(TL-1)]=0;
        for (i=0; i<TL-1; ++i) {
            Tripletas[k/(TL-1)]|=(1<<Vertices[(k/(TL-1))*TL+i]);
        }
        Tripletas[k/(TL-1)]|=(1<<a);
        Vertices[(k/(TL-1))*TL+TL-1]=a;
        minimo=Vertices[(k/(TL-1))*TL];
        while (buscarvertice(Tripletas, k+1, minimo, TL)==1)
            minimo++;
        Vertices[(k/(TL-1))*TL+TL]=minimo;
        S[k-TL*(TL-1)+1]=minimo+1;
    }
    else{
        Vertices[(k/(TL-1))*TL+k%(TL-1)+1]=a;
        S[k-TL*(TL-1)+1]=a+1;
    }
}
void graficar(graph *g, int TG, int *Vertices, int terminado, int NV, int TL){
    int i, j, k;
    EMPTYGRAPH(g,1,TG);
    for (i=0; i<terminado; i++){
        for (j=0; j<TL-1; ++j)
            for (k=j+1; k<TL; ++k)
                ADDONEEDGE(g, Vertices[i*TL+j], Vertices[i*TL+k], 1);
        for (j=0; j<TL; ++j)
            ADDONEEDGE(g, Vertices[i*TL+j], NV+i, 1);
    }
}
void inicilisar(int *Vertices, int *Tripletas, int TL){
    int i, j, k=0;
    for (i=0; i<TL; ++i) {
        Tripletas[i]=0;
        Tripletas[i]|=(1<<0);
        Vertices[i*TL]=0;
        for (j=1; j<TL; ++j) {
            Vertices[i*TL+j]=++k;
            Tripletas[i]|=(1<<k);
        }
    }
    Vertices[TL*TL]=1;
}
int configuraciones(int NV, int TL, float LM) {
    struct Canon *BDG[NV-TL];
    int contador[NV-TL], aceptado[NV-TL], lab[MAXN], ptn[MAXN], orbits[MAXN], Tripletas[NV], S[52], Vertices[80], i, k=TL*(TL-1), a, PT=0, TG=2*NV;
    graph g[MAXN*MAXM], canon[MAXN*MAXM];
    float limite=1000000000/(sizeof(struct Canon)+TG*sizeof(graph))*LM;
    struct rusage ru_begin;
    struct rusage ru_end;
    struct timeval tv_elapsed;
    getrusage(RUSAGE_SELF, &ru_begin);
    nauty_check(WORDSIZE,1,MAXN,NAUTYVERSIONID);
    static DEFAULTOPTIONS_GRAPH(options);
    options.getcanon = TRUE;
    options.defaultptn=FALSE;
    statsblk stats;
    for (i=0; i<MAXN; i++) {
        lab[i]=i;
        ptn[i]=1;
    }
    inicilisar(Vertices, Tripletas, TL);
    S[0]=TL;
    for (i=0; i<NV-TL; ++i)
        BDG[i]=NULL;
    ptn[NV-1]=0;
    ptn[TG-1]=0;
    printf("Soluciones:\n");
    for (i=0; i<NV-TL; i++){
        contador[i]=0;
        aceptado[i]=1;
    }
    while (k>TL*(TL-1)-1) {
        while (S[k-TL*(TL-1)]<NV) {
            a=S[k-TL*(TL-1)];
            S[k-TL*(TL-1)]++;
            //printf("\na=%i, k=%i, Vertices[%i]=(%i, %i, %i)\n", a, k, k/2, Vertices[k/2], Vertices[k/2+1], Vertices[k/2+2]);
            //imprimirtripleta(Vertices);
            if (verificar(Tripletas, Vertices, k, a, TL)==0) {
                actualizar(Tripletas, Vertices, S, k, a, TL);
                if (k%(TL-1)==TL-2 && aceptado[k/(TL-1)-TL]==1) {
                    graficar(g, TG, Vertices, k/(TL-1)+1, NV, TL);
                    densenauty(g, lab, ptn, orbits, &options, &stats, 1, TG, canon);
                    i=Nuevo(&BDG[k/(TL-1)-TL], canon, TG);
                    if(i==0){
                        ++contador[k/(TL-1)-TL];
                        ++PT;
                        if (k==NV*(TL-1)-1 && contador[k/(TL-1)-TL]%1000000==0) {
                            imprimirtripleta(Vertices, NV, TL);
                            getrusage(RUSAGE_SELF, &ru_end);
                            timeval_subtract(&tv_elapsed, &ru_end.ru_utime, &ru_begin.ru_utime);
                            printf("\nN=%i Tiempo %g ms.", contador[NV-TL-1], (tv_elapsed.tv_sec + (tv_elapsed.tv_usec/1000000.0))*1000.0);
                        }
                        else
                            ++k;
                        if (PT>limite) {
                            a=0;
                            for (i=1; i<NV-TL; ++i)
                                if (aceptado[i]==1 && contador[a]<contador[i])
                                    a=i;
                            if(a==NV-TL-1){
                                printf("\nSe necesita mas memoria");
                                for (i=0; i<NV-TL; ++i)
                                    Borrar(BDG[i]);
                                return 0;
                            }
                            else{
                                aceptado[a]=0;
                                Borrar(BDG[a]);
                                PT=PT-contador[a];
                                BDG[a]=NULL;
                            }
                        }
                    }
                    else{
                        if (i==2){
                            printf("\nError: Falta memoria");
                            for (i=0; i<NV-TL; ++i)
                                Borrar(BDG[i]);
                            return 0;
                        }
                    }
                }
                else
                    ++k;
            }
        }
        k--;
    }
    for (i=0; i<NV-TL; ++i)
        Borrar(BDG[i]);
    printf("\nTotal: %i", contador[NV-TL-1]);
    getrusage(RUSAGE_SELF, &ru_end);
    timeval_subtract(&tv_elapsed, &ru_end.ru_utime, &ru_begin.ru_utime);
    printf("\nTiempo %g ms.\n", (tv_elapsed.tv_sec + (tv_elapsed.tv_usec/1000000.0))*1000.0);
    for (i=0; i<NV-TL; ++i){
        printf("\nPila %i llego al tamaño de %i elementos", i, contador[i]);
        if (aceptado[i]==0)
            printf(" y se lleno la memoria");
    }
    printf("\ntam %i", sizeof(struct Canon)+TG*sizeof(graph));
    return 0;
}
int main(int argc, const char * argv[]) {
    int NV, TL;
    float LM;
    char continuar='s';
    printf("¿Limite de la memoria en Gigabytes?\n");
    scanf("%f", &LM);
    while (continuar=='s') {
        printf("¿Cuantos vertices?\n");
        scanf("%d", &NV);
        printf("¿Cuantos vertices por linea?\n");
        scanf("%d", &TL);
        configuraciones(NV, TL, LM);
        printf("\n¿Desea continuar?\n");
        scanf("%s", &continuar);
    }
}