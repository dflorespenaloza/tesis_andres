#define ENCONTRADO 1
#define NO_ENCONTRADO 0
#define VALIDO 0
#define INVALIDO 1
#define MENOR -1
#define IGUAL 0
#define MAYOR 1
#include "nauty.h"
#include "profile_time.h"
#include <stdio.h>
struct nodo{
    graph *canon;
    int *grafica;
    struct nodo *menores;
    struct nodo *mayores;
};
int comparargraficas(graph *g1, graph *g2,int  tgrafica){
    size_t k;
    for (k = 0; k < (size_t)tgrafica; ++k){
        if (g1[k] < g2[k])
            return MENOR;
        if (g1[k] > g2[k])
            return MAYOR;
    }
    return IGUAL;
}
int buscargrafica(struct nodo *pi, graph *g, int tgrafica, struct nodo **padre, int *comparacion){
    while (pi!=NULL) {
        *padre=pi;
        *comparacion=comparargraficas(g, pi->canon, tgrafica);
        if (*comparacion==IGUAL)
            return ENCONTRADO;
        if (*comparacion==MAYOR)
            pi=pi->mayores;
        else if (*comparacion==MENOR)
            pi=pi->menores;
    }
    return NO_ENCONTRADO;
}
int encontrar_o_agregar(struct nodo **raiz, graph *canon, int *grafica, int tgrafica){
    struct nodo *padre;
    int comparacion;
    if(buscargrafica(*raiz, canon, tgrafica, &padre, &comparacion)==NO_ENCONTRADO){
        size_t k;
        struct nodo *N= (struct nodo *)malloc(sizeof(struct nodo));
        if (N==NULL)
            return 2;
        N->canon=(graph *)malloc(sizeof(graph)*tgrafica);
        if (N->canon==NULL){
            free(N);
            return 2;
        }
        N->grafica=(int *)malloc(sizeof(int)*tgrafica);
        if (N->grafica==NULL){
            free(N->canon);
            free(N);
            return 2;
        }
        N->menores=NULL;
        N->mayores=NULL;
        for (k = 0; k < (size_t)tgrafica; ++k){
            N->canon[k]=canon[k];
            N->grafica[k]=grafica[k];
        }
        if (*raiz==NULL)
            *raiz=N;
        else{
            if (comparacion==MAYOR)
                padre->mayores=N;
            else
                padre->menores=N;
        }
        return NO_ENCONTRADO;
    }
    return ENCONTRADO;
}
void Borrar(struct nodo *pi){
    if(pi!=NULL){
        Borrar(pi->mayores);
        Borrar(pi->menores);
        free(pi->grafica);
        free(pi->canon);
        free(pi);
    }
}
void imprimirlineas(int nvertices, int tlinea, int *lineas, int tam){
    int i, j, nencontrados;
    for (i=0; i<tam; ++i) {
        printf("(");
        for (j=0, nencontrados=0; j<nvertices && nencontrados<tlinea; ++j){
            if (lineas[i]&(1<<j)){
                ++nencontrados;
                printf("%i", j+1);
                if (nencontrados<tlinea)
                    printf(" ");
            }
        }
        printf(")");
    }
    printf("\n");
}
void imprimirvertices(int nvertices, int tlinea, int *vertices, int tam){
    int i, j;
    for (i=0; i<tam; ++i) {
        printf("(");
        for (j=0; j<tlinea; ++j){
            printf("%i", vertices[i*tlinea+j]);
            if (j<tlinea-1)
                printf(" ");
        }
        printf(")");
    }
    printf("\n");
}
int buscarpar(int *lineas, int paso, int a, int b){
    int i;
    b=(1<<b);
    b|=(1<<a);
    for (i=0; i<paso; i++)
        if ((lineas[i]&b)==b)
            return ENCONTRADO;
    return NO_ENCONTRADO;
}
int buscarvertice(int *lineas, int paso, int vertice, int tlinea){
    int i, contador;
    for (i=0, contador=0; i<paso; i++)
        if (lineas[i]&(1<<vertice)){
            contador++;
            if (contador==tlinea)
                return ENCONTRADO;
        }
    return NO_ENCONTRADO;
}
int validar(int *lineas, int *linea, int k, int vertice, int paso, int tlinea){
    if(buscarvertice(lineas, paso, vertice, tlinea)==ENCONTRADO)
        return INVALIDO;
    for (int i=0; i<k; ++i)
        if (buscarpar(lineas, paso, linea[i], vertice)==ENCONTRADO)
            return INVALIDO;
    return VALIDO;
}
void graficar(graph *g, int tgrafica, int *lineas, int terminado, int nvertices, int tlinea){
    int i, j, k;
    EMPTYGRAPH(g,1,tgrafica);
    for (i=0; i<terminado; i++)
        for (j=0; j<nvertices; ++j)
            if (lineas[i]&(1<<j)){
                ADDONEEDGE(g, j, nvertices+i, 1);
                for (k=j+1; k<nvertices; ++k)
                    if (lineas[i]&(1<<k))
                        ADDONEEDGE(g, j, k, 1);
            }
}
void paso(int *lineas, int *lab, int *ptn, int *orbits, int nvertices, int tlinea, int paso, struct nodo **arbol, int *contar){
    int k=1, i, e;
    int linea[tlinea], S[tlinea];
    graph solucion[2*nvertices], canon[2*nvertices];
    nauty_check(WORDSIZE,1,MAXN,NAUTYVERSIONID);
    static DEFAULTOPTIONS_GRAPH(options);
    options.getcanon = TRUE;
    options.defaultptn=FALSE;
    statsblk stats;
    for (i=0; i<nvertices && (!(lineas[paso-1]&(1<<i))); ++i);
    while (buscarvertice(lineas, paso, i, tlinea)==ENCONTRADO)
        ++i;
    linea[0]=i;
    S[1]=linea[0]+1;
    while (k>0) {
        while (S[k]<=nvertices-tlinea+k) {
            linea[k]=S[k];
            //printf("k=%i v=%i", k, vertice);
            ++S[k];
            if (validar(lineas, linea, k, linea[k], paso, tlinea)==VALIDO) {
                if (k==tlinea-1){
                    lineas[paso]=0;
                    for (i=0; i<tlinea; ++i)
                        lineas[paso]|=(1<<linea[i]);
                    graficar(solucion, 2*nvertices, lineas, paso+1, nvertices, tlinea);
                    densenauty(solucion, lab, ptn, orbits, &options, &stats, 1, 2*nvertices, canon);
                    e=encontrar_o_agregar(arbol, canon, lineas, 2*nvertices);
                    if (e!=ENCONTRADO){
                        ++*contar;
                        //imprimirlineas(nvertices, tlinea, lineas, paso+1);
                    }
                }
                else{
                    ++k;
                    S[k]=S[k-1];
                }
            }
        }
        --k;
    }
}
void navegar(struct nodo *P, int *lineas,int *lab, int *ptn, int *orbits, int nvertices, int tlinea, int npaso, struct nodo **arbol, int *contar){
    if (P!=NULL) {
        navegar(P->mayores, lineas, lab, ptn, orbits, nvertices, tlinea, npaso, arbol, contar);
        navegar(P->menores, lineas, lab, ptn, orbits, nvertices, tlinea, npaso, arbol, contar);
        for (int i=0; i<npaso; ++i)
            lineas[i]=P->grafica[i];
        //printf("hola");
        paso(lineas, lab, ptn, orbits, nvertices, tlinea, npaso, arbol, contar);
    }
}
//sistema resoluble por pasos
void SRP(int nvertices, int tlinea){
    int lineas[nvertices], vertices[nvertices*tlinea], lab[2*nvertices], ptn[2*nvertices], orbits[2*nvertices];
    int i, k, a, contar;
    struct nodo *arbol1=NULL, *arbol2=NULL, *cambio=NULL;
    for (i=0, k=0; i<tlinea; ++i) {
        lineas[i]=0;
        lineas[i]|=(1<<0);
        vertices[i*tlinea]=0;
        for (a=1; a<tlinea; ++a) {
            vertices[i*tlinea+a]=++k;
            lineas[i]|=(1<<k);
        }
    }
    vertices[tlinea*tlinea]=1;
    for (i=0; i<2*nvertices; i++){
        ptn[i]=1;
        lab[i]=i;
    }
    paso(lineas, lab, ptn, orbits, nvertices, tlinea, tlinea, &arbol1, &contar);
    for (i=tlinea+1; i<nvertices; ++i){
        contar=0;
        navegar(arbol1, lineas, lab, ptn, orbits, nvertices, tlinea, i, &arbol2, &contar);
        printf("%i soluciones en la etapa %i\n", contar, i-tlinea);
        cambio=arbol1;
        arbol1=arbol2;
        arbol2=cambio;
        Borrar(arbol2);
        arbol2=NULL;
    }
    Borrar(arbol1);
    Borrar(arbol2);
}
int main(int argc, const char * argv[]) {
    float limite;
    int nvertices, tlinea;
    char continuar='s';
    while(continuar=='s'){
        /*********************************************
                    Se obtienen los datos
         *********************************************/
        do {
            printf("¿limite de la memoria en Gigabytes?\n");
            scanf("%f", &limite);
            if (limite<=0)
                printf("Se necesita un valor positivo\n");
        } while (limite<=0);
        do{
            printf("¿Cuantos vertices?\n");
            scanf("%d", &nvertices);
            if (nvertices>20 || nvertices<7)
                printf("Solo valores entre 7 y 20");
        }while(nvertices>20 || nvertices<7);
        do{
            printf("¿Cuantos vertices por linea?\n");
            scanf("%d", &tlinea);
            if(tlinea!=3 && tlinea!=4)
                printf("Solo 3 o 4");
        }while (tlinea!=3 && tlinea!=4);
        /************************************************
                Se inicia el proceso
         ************************************************/
        SRP(nvertices, tlinea);
        printf("\n¿Desea continuar?\n");
        scanf("%s", &continuar);
    }
    return 0;
}
