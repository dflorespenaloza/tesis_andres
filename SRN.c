#define MAXN 40// numero maximo de vertices
//#define MAXN 2*Num
#include "nauty.h"
#include "profile_time.h"
struct nodo{
    graph *C;
    struct nodo *menores;
    struct nodo *mayores;
};
int comparargraficas(graph *C1, graph *C2,int  TamGrafica){
    //regresa 0 si son igules, 1 si C1 es mayor
    //que C2 y -1 si C1 es menor que C2.
    size_t k;
    for (k = 0; k < (size_t)TamGrafica; ++k){
        if (C1[k] < C2[k])
            return 1;
        if (C1[k] > C2[k])
            return -1;
    }
    return 0;
}
int buscargrafica(struct nodo *pi, graph *C, int TamGrafica, struct nodo **padre, int *comparacion){
    //0 si la grafica es nueva, 1 si es repetido
    while (pi!=NULL) {
        *padre=pi;
        *comparacion=comparargraficas(C, pi->C, TamGrafica);
        if (*comparacion==0)
            return 1;
        if (*comparacion==1)
            pi=pi->mayores;
        else if (*comparacion==-1)
            pi=pi->menores;
    }
    return 0;
}
int Nuevo(struct nodo **raiz, graph *C, int TamGrafica){
    //Da 0 si se agrego el nodo.
    //Da 1 sino.
    struct nodo *padre;
    int comparacion;
    if(buscargrafica(*raiz, C, TamGrafica, &padre, &comparacion)==0){
        size_t k;
        struct nodo *N= (struct nodo *)malloc(sizeof(struct nodo));
        if (N==NULL)
            return 2;
        N->C=(graph *)malloc(sizeof(graph)*TamGrafica);
        N->menores=NULL;
        N->mayores=NULL;
        for (k = 0; k < (size_t)TamGrafica; ++k)
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
void Borrar(struct nodo *pi){
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
void imprimirtripleta(int *Vertices, int NumVertices, int TamLinea){
    int i, j;
    printf("\n");
    for (i=0; i<NumVertices; i++){
        printf("(");
        for (j=0; j<TamLinea-1; j++)
            printf("%i, ", Vertices[TamLinea*i+j]+1);
        printf("%i)", Vertices[TamLinea*i+TamLinea-1]+1);
    }
}
int buscarvertice(int *Tripletas, int k, int a, int TamLinea){
    int i, contador;
    for (i=0, contador=0; i<k/(TamLinea-1); i++)
        if (Tripletas[i]&(1<<a)){
            contador++;
            if (contador==TamLinea)
                return 1;
        }
    return 0;
}
int verificar(int *Tripletas, int *Vertices, int k, int  a, int TamLinea){
    int i;
    if(buscarvertice(Tripletas, k, a, TamLinea)==1)
        return 1;
    if (k%(TamLinea-1)==0){
        if (Vertices[(k/(TamLinea-1))*TamLinea-TamLinea]==Vertices[(k/(TamLinea-1))*TamLinea]&&a<Vertices[(k/(TamLinea-1))*TamLinea-TamLinea+1])
            return 1;
    }
    for (i=0; i<=k%(TamLinea-1); i++) {
        if(buscarpar(Tripletas, k/(TamLinea-1), Vertices[(k/(TamLinea-1))*TamLinea+i], a)==1)
            return 1;
    }
    return 0;
}
void actualizar(int *Tripletas, int *Vertices, int *S, int k, int  a, int TamLinea){
    if (k%(TamLinea-1)==TamLinea-2) {
        int minimo, i;
        Tripletas[k/(TamLinea-1)]=0;
        for (i=0; i<TamLinea-1; ++i) {
            Tripletas[k/(TamLinea-1)]|=(1<<Vertices[(k/(TamLinea-1))*TamLinea+i]);
        }
        Tripletas[k/(TamLinea-1)]|=(1<<a);
        Vertices[(k/(TamLinea-1))*TamLinea+TamLinea-1]=a;
        minimo=Vertices[(k/(TamLinea-1))*TamLinea];
        while (buscarvertice(Tripletas, k+1, minimo, TamLinea)==1)
            minimo++;
        Vertices[(k/(TamLinea-1))*TamLinea+TamLinea]=minimo;
        S[k-TamLinea*(TamLinea-1)+1]=minimo+1;
    }
    else{
        Vertices[(k/(TamLinea-1))*TamLinea+k%(TamLinea-1)+1]=a;
        S[k-TamLinea*(TamLinea-1)+1]=a+1;
    }
}
void graficar(graph *g, int TamGrafica, int *Vertices, int terminado, int NumVertices, int TamLinea){
    int i, j, k;
    EMPTYGRAPH(g,1,TamGrafica);
    for (i=0; i<terminado; i++){
        for (j=0; j<TamLinea-1; ++j)
            for (k=j+1; k<TamLinea; ++k)
                ADDONEEDGE(g, Vertices[i*TamLinea+j], Vertices[i*TamLinea+k], 1);
        for (j=0; j<TamLinea; ++j)
            ADDONEEDGE(g, Vertices[i*TamLinea+j], NumVertices+i, 1);
    }
}
void inicilisar(int *TamGrafica, int *S, int *Vertices, int *Tripletas, int TamLinea, int NumVertices){
    int i, j, k=0;
    *TamGrafica=2*NumVertices;
    S[0]=TamLinea;
    for (i=0; i<TamLinea; ++i) {
        Tripletas[i]=0;
        Tripletas[i]|=(1<<0);
        Vertices[i*TamLinea]=0;
        for (j=1; j<TamLinea; ++j) {
            Vertices[i*TamLinea+j]=++k;
            Tripletas[i]|=(1<<k);
        }
    }
    Vertices[TamLinea*TamLinea]=1;
}
void obtenerdatos(int *NumVertices, int *TamLinea, float *Limite, int *salto) {
    do {
        printf("¿Limite de la memoria en Gigabytes?\n");
        scanf("%f", Limite);
        if (*Limite<=0)
            printf("Se necesita un valor positivo\n");
    } while (*Limite<=0);
    do{
        printf("¿Cuantos vertices?\n");
        scanf("%d", NumVertices);
        if (*NumVertices<=0)
            printf("Se necesita un valor positivo\n");
    } while (*NumVertices<=0);
    do{
        printf("¿Cuantos vertices por linea?\n");
        scanf("%d", TamLinea);
        if (*TamLinea<=0)
            printf("Se necesita un valor positivo\n");
    } while (*TamLinea<=0);
    do{
        printf("¿Cada cuantas soluciones se imprime el progreso?\n");
        scanf("%d", salto);
        if (*salto<=0)
            printf("Se necesita un valor positivo\n");
    } while (*salto<=0);
}
int main(int argc, const char * argv[]) {
    struct nodo *corteisomorfismo[17];
    int contador[17], aceptado[17], lab[MAXN], ptn[MAXN], orbits[MAXN], Tripletas[20], S[52], Vertices[80], NumVertices, TamLinea, i, k, a, UnionArboles, TamGrafica, salto;
    graph g[MAXN*MAXM], canon[MAXN*MAXM];
    float Limite;
    char continuar='s';
    for (i=0; i<MAXN; i++)
        ptn[i]=1;
    struct rusage ru_begin;
    struct rusage ru_end;
    struct timeval tv_elapsed;
    nauty_check(WORDSIZE,1,MAXN,NAUTYVERSIONID);
    static DEFAULTOPTIONS_GRAPH(options);
    options.getcanon = TRUE;
    options.defaultptn=FALSE;
    statsblk stats;
    while(continuar=='s'){
        getrusage(RUSAGE_SELF, &ru_begin);
        obtenerdatos(&NumVertices, &TamLinea, &Limite, &salto);
        UnionArboles=0;
        k=TamLinea*(TamLinea-1);
        inicilisar(&TamGrafica, S, Vertices, Tripletas, TamLinea, NumVertices);
        Limite*=1048576000/(sizeof(struct nodo)+TamGrafica*sizeof(graph))*0.975;
        for (i=0; i<TamGrafica; i++)
            lab[i]=i;
        for (i=0; i<NumVertices-TamLinea; ++i)
            corteisomorfismo[i]=NULL;
        ptn[NumVertices-1]=0;
        ptn[TamGrafica-1]=0;
        printf("Soluciones:\n");
        for (i=0; i<NumVertices-TamLinea; i++){
            contador[i]=0;
            aceptado[i]=1;
        }
        while (k>TamLinea*(TamLinea-1)-1) {
            while (S[k-TamLinea*(TamLinea-1)]<NumVertices) {
                a=S[k-TamLinea*(TamLinea-1)];
                S[k-TamLinea*(TamLinea-1)]++;
                //printf("\na=%i, k=%i, Vertices[%i]=(%i, %i, %i)\n", a, k, k/2, Vertices[k/2], Vertices[k/2+1], Vertices[k/2+2]);
                //imprimirtripleta(Vertices);
                if (verificar(Tripletas, Vertices, k, a, TamLinea)==0) {
                    actualizar(Tripletas, Vertices, S, k, a, TamLinea);
                    if (k%(TamLinea-1)==TamLinea-2 && aceptado[k/(TamLinea-1)-TamLinea]==1) {
                        graficar(g, TamGrafica, Vertices, k/(TamLinea-1)+1, NumVertices, TamLinea);
                        densenauty(g, lab, ptn, orbits, &options, &stats, 1, TamGrafica, canon);
                        i=Nuevo(&corteisomorfismo[k/(TamLinea-1)-TamLinea], canon, TamGrafica);
                        if(i==0){
                            ++contador[k/(TamLinea-1)-TamLinea];
                            ++UnionArboles;
                            if (k==NumVertices*(TamLinea-1)-1 && contador[k/(TamLinea-1)-TamLinea]%salto==0) {
                                //imprimirtripleta(Vertices, NumVertices, TamLinea);
                                getrusage(RUSAGE_SELF, &ru_end);
                                timeval_subtract(&tv_elapsed, &ru_end.ru_utime, &ru_begin.ru_utime);
                                printf("\n%i soluciones encontradas. Tiempo %g ms.", contador[NumVertices-TamLinea-1], (tv_elapsed.tv_sec + (tv_elapsed.tv_usec/1000000.0))*1000.0);
                            }
                            else
                                ++k;
                            if (UnionArboles>Limite) {
                                a=0;
                                for (i=1; i<NumVertices-TamLinea; ++i)
                                    if (aceptado[i]==1 && contador[a]<contador[i])
                                        a=i;
                                if(a==NumVertices-TamLinea-1){
                                    printf("\nSe necesita mas memoria");
                                    for (i=0; i<NumVertices-TamLinea; ++i)
                                        Borrar(corteisomorfismo[i]);
                                    return 0;
                                }
                                else{
                                    aceptado[a]=0;
                                    Borrar(corteisomorfismo[a]);
                                    UnionArboles=-contador[a];
                                    corteisomorfismo[a]=NULL;
                                }
                            }
                        }
                        else{
                            if (i==2){
                                printf("\nError: Falta memoria");
                                for (i=0; i<NumVertices-TamLinea; ++i)
                                    Borrar(corteisomorfismo[i]);
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
        ptn[NumVertices-1]=1;
        ptn[TamGrafica-1]=1;
        for (i=0; i<NumVertices-TamLinea; ++i)
            Borrar(corteisomorfismo[i]);
        printf("\nTotal: %i", contador[NumVertices-TamLinea-1]);
        getrusage(RUSAGE_SELF, &ru_end);
        timeval_subtract(&tv_elapsed, &ru_end.ru_utime, &ru_begin.ru_utime);
        printf("\nTiempo %g ms.\n", (tv_elapsed.tv_sec + (tv_elapsed.tv_usec/1000000.0))*1000.0);
        for (i=0; i<NumVertices-TamLinea; ++i){
            printf("\nPila %i llego al tamaño de %i elementos", i, contador[i]);
            if (aceptado[i]==0)
                printf(" y se lleno la memoria");
        }
        printf("\n¿Desea continuar?\n");
        scanf("%s", &continuar);
    }
    return 0;
}