#define MAXN 40// Tamaño maximo de la grafiva
#include "nauty.h"
#include "profile_time.h"
struct nodo{
    graph *C;
    struct nodo *menores;
    struct nodo *mayores;
};
int comparargraficas(graph *C1, graph *C2,int  tgrafica){
    //regresa 0 si son igules, 1 si C1 es mayor que C2
    //y -1 si C1 es menor que C2.
    size_t k;
    for (k = 0; k < (size_t)tgrafica; ++k){
        if (C1[k] < C2[k])
            return 1;
        if (C1[k] > C2[k])
            return -1;
    }
    return 0;
}
int buscargrafica(struct nodo *pi, graph *C, int tgrafica, struct nodo **padre, int *comparacion){
    //0 si la grafica es nueva, 1 si es repetido
    while (pi!=NULL) {
        *padre=pi;
        *comparacion=comparargraficas(C, pi->C, tgrafica);
        if (*comparacion==0)
            return 1;
        if (*comparacion==1)
            pi=pi->mayores;
        else if (*comparacion==-1)
            pi=pi->menores;
    }
    return 0;
}
int Nuevo(struct nodo **raiz, graph *C, int tgrafica){
    //Da 0 si se agrego el nodo.
    //Da 1 sino.
    struct nodo *padre;
    int comparacion;
    if(buscargrafica(*raiz, C, tgrafica, &padre, &comparacion)==0){
        size_t k;
        struct nodo *N= (struct nodo *)malloc(sizeof(struct nodo));
        if (N==NULL)
            return 2;
        N->C=(graph *)malloc(sizeof(graph)*tgrafica);
        N->menores=NULL;
        N->mayores=NULL;
        for (k = 0; k < (size_t)tgrafica; ++k)
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
int buscarpar(int *tripletas, int tam, int a, int b){
    int i;
    b=(1<<b);
    b|=(1<<a);
    for (i=0; i<tam; i++)
        if ((tripletas[i]&b)==b)
            return 1;
    return 0;
}
void imprimirtripleta(int *vertices, int nvertices, int tlinea){
    int i, j;
    printf("\n");
    for (i=0; i<nvertices; i++){
        printf("(");
        for (j=0; j<tlinea-1; j++)
            printf("%i, ", vertices[tlinea*i+j]+1);
        printf("%i)", vertices[tlinea*i+tlinea-1]+1);
    }
}
int buscarvertice(int *tripletas, int k, int a, int tlinea){
    int i, contador;
    for (i=0, contador=0; i<k/(tlinea-1); i++)
        if (tripletas[i]&(1<<a)){
            contador++;
            if (contador==tlinea)
                return 1;
        }
    return 0;
}
int verificar(int *tripletas, int *vertices, int k, int  a, int tlinea){
    int i;
    if(buscarvertice(tripletas, k, a, tlinea)==1)
        return 1;
    if (k%(tlinea-1)==0){
        if (vertices[(k/(tlinea-1))*tlinea-tlinea]==vertices[(k/(tlinea-1))*tlinea]&&a<vertices[(k/(tlinea-1))*tlinea-tlinea+1])
            return 1;
    }
    for (i=0; i<=k%(tlinea-1); i++) {
        if(buscarpar(tripletas, k/(tlinea-1), vertices[(k/(tlinea-1))*tlinea+i], a)==1)
            return 1;
    }
    return 0;
}
void actualizar(int *tripletas, int *vertices, int *S, int k, int  a, int tlinea){
    if (k%(tlinea-1)==tlinea-2) {
        int minimo, i;
        tripletas[k/(tlinea-1)]=0;
        for (i=0; i<tlinea-1; ++i) {
            tripletas[k/(tlinea-1)]|=(1<<vertices[(k/(tlinea-1))*tlinea+i]);
        }
        tripletas[k/(tlinea-1)]|=(1<<a);
        vertices[(k/(tlinea-1))*tlinea+tlinea-1]=a;
        minimo=vertices[(k/(tlinea-1))*tlinea];
        while (buscarvertice(tripletas, k+1, minimo, tlinea)==1)
            minimo++;
        vertices[(k/(tlinea-1))*tlinea+tlinea]=minimo;
        S[k-tlinea*(tlinea-1)+1]=minimo+1;
    }
    else{
        vertices[(k/(tlinea-1))*tlinea+k%(tlinea-1)+1]=a;
        S[k-tlinea*(tlinea-1)+1]=a+1;
    }
}
void graficar(graph *g, int tgrafica, int *vertices, int terminado, int nvertices, int tlinea){
    int i, j, k;
    EMPTYGRAPH(g,1,tgrafica);
    for (i=0; i<terminado; i++){
        for (j=0; j<tlinea-1; ++j)
            for (k=j+1; k<tlinea; ++k)
                ADDONEEDGE(g, vertices[i*tlinea+j], vertices[i*tlinea+k], 1);
        for (j=0; j<tlinea; ++j)
            ADDONEEDGE(g, vertices[i*tlinea+j], nvertices+i, 1);
    }
}
void inicilisar(int *tgrafica, int *tnoisomorfos, int *S, int *vertices, int *tripletas, int tlinea, int nvertices){
    int i, j, k=0;
    *tgrafica=2*nvertices;
    S[0]=tlinea;
    *tnoisomorfos=0;
    for (i=0; i<tlinea; ++i) {
        tripletas[i]=0;
        tripletas[i]|=(1<<0);
        vertices[i*tlinea]=0;
        for (j=1; j<tlinea; ++j) {
            vertices[i*tlinea+j]=++k;
            tripletas[i]|=(1<<k);
        }
    }
    vertices[tlinea*tlinea]=1;
}
void obtenerdatos(int *nvertices, int *tlinea, float *limite, int *salto) {
    do {
        printf("¿limite de la memoria en Gigabytes?\n");
        scanf("%f", limite);
        if (*limite<=0)
            printf("Se necesita un valor positivo\n");
    } while (*limite<=0);
    do{
        printf("¿Cuantos vertices?\n");
        scanf("%d", nvertices);
        if (*nvertices<=0)
            printf("Se necesita un valor positivo\n");
    } while (*nvertices<=0);
    do{
        printf("¿Cuantos vertices por linea?\n");
        scanf("%d", tlinea);
        if (*tlinea<=0)
            printf("Se necesita un valor positivo\n");
    } while (*tlinea<=0);
    do{
        printf("¿Cada cuantas soluciones se imprime el progreso?\n");
        scanf("%d", salto);
        if (*salto<=0)
            printf("Se necesita un valor positivo\n");
    } while (*salto<=0);
}
int main(int argc, const char * argv[]) {
    /**********************************
     Fase 1 inicialisacion
     **********************************/
    struct nodo *csni[17];//Contenedor de soluciones no isomorfas
    int contador[17], aceptado[17], lab[MAXN], ptn[MAXN], orbits[MAXN], tripletas[20], s[52], vertices[80], nvertices, tlinea, i, k, a, tnoisomorfos, tgrafica, salto;
    graph g[MAXN*MAXM], canon[MAXN*MAXM];
    //lab, optn, orbits, g y canon son nombres tomados del ejemplo en el manual de nauty y tienen el mismo rol
    float limite;
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
        obtenerdatos(&nvertices, &tlinea, &limite, &salto);
        inicilisar(&tgrafica, &tnoisomorfos, s, vertices, tripletas, tlinea, nvertices);
        limite*=1048576000/(sizeof(struct nodo)+tgrafica*sizeof(graph))*0.975;
        for (i=0; i<tgrafica; i++)
            lab[i]=i;
        for (i=0; i<nvertices-tlinea; ++i)
            csni[i]=NULL;
        ptn[nvertices-1]=0;
        ptn[tgrafica-1]=0;
        printf("Soluciones:\n");
        for (i=0; i<nvertices-tlinea; i++){
            contador[i]=0;
            aceptado[i]=1;
        }
        k=tlinea*(tlinea-1);
        while (k>tlinea*(tlinea-1)-1) {
            while (s[k-tlinea*(tlinea-1)]<nvertices) {
                a=s[k-tlinea*(tlinea-1)];
                s[k-tlinea*(tlinea-1)]++;
                //printf("\na=%i, k=%i, vertices[%i]=(%i, %i, %i)\n", a, k, k/2, vertices[k/2], vertices[k/2+1], vertices[k/2+2]);
                //imprimirtripleta(vertices);
                if (verificar(tripletas, vertices, k, a, tlinea)==0) {
                    actualizar(tripletas, vertices, s, k, a, tlinea);
                    if (k%(tlinea-1)==tlinea-2 && aceptado[k/(tlinea-1)-tlinea]==1) {
                        graficar(g, tgrafica, vertices, k/(tlinea-1)+1, nvertices, tlinea);
                        densenauty(g, lab, ptn, orbits, &options, &stats, 1, tgrafica, canon);
                        i=Nuevo(&csni[k/(tlinea-1)-tlinea], canon, tgrafica);
                        if(i==0){
                            ++contador[k/(tlinea-1)-tlinea];
                            ++tnoisomorfos;
                            if (k==nvertices*(tlinea-1)-1 && contador[k/(tlinea-1)-tlinea]%salto==0) {
                                //imprimirtripleta(vertices, nvertices, tlinea);
                                getrusage(RUSAGE_SELF, &ru_end);
                                timeval_subtract(&tv_elapsed, &ru_end.ru_utime, &ru_begin.ru_utime);
                                printf("\n%i soluciones encontradas. Tiempo %g ms.", contador[nvertices-tlinea-1], (tv_elapsed.tv_sec + (tv_elapsed.tv_usec/1000000.0))*1000.0);
                            }
                            else
                                ++k;
                            if (tnoisomorfos>limite) {
                                a=0;
                                for (i=1; i<nvertices-tlinea; ++i)
                                    if (aceptado[i]==1 && contador[a]<contador[i])
                                        a=i;
                                if(a==nvertices-tlinea-1){
                                    printf("\nSe necesita mas memoria");
                                    for (i=0; i<nvertices-tlinea; ++i)
                                        Borrar(csni[i]);
                                    return 0;
                                }
                                else{
                                    aceptado[a]=0;
                                    Borrar(csni[a]);
                                    tnoisomorfos=-contador[a];
                                    csni[a]=NULL;
                                }
                            }
                        }
                        else{
                            if (i==2){
                                printf("\nError: Falta memoria");
                                for (i=0; i<nvertices-tlinea; ++i)
                                    Borrar(csni[i]);
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
        ptn[nvertices-1]=1;
        ptn[tgrafica-1]=1;
        for (i=0; i<nvertices-tlinea; ++i)
            Borrar(csni[i]);
        printf("\nTotal: %i", contador[nvertices-tlinea-1]);
        getrusage(RUSAGE_SELF, &ru_end);
        timeval_subtract(&tv_elapsed, &ru_end.ru_utime, &ru_begin.ru_utime);
        printf("\nTiempo %g ms.\n", (tv_elapsed.tv_sec + (tv_elapsed.tv_usec/1000000.0))*1000.0);
        for (i=0; i<nvertices-tlinea; ++i){
            printf("\nPila %i llego al tamaño de %i elementos", i, contador[i]);
            if (aceptado[i]==0)
                printf(" y se lleno la memoria");
        }
        printf("\n¿Desea continuar?\n");
        scanf("%s", &continuar);
    }
    return 0;
}