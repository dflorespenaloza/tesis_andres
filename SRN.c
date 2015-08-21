#define MAXN 40// Tamaño maximo de la grafiva
#define ENCONTRADO 1
#define NO_ENCONTRADO 0
#define MENOR -1
#define IGUAL 0
#define MAYOR 1
#define VALIDO 0
#define INVALIDO 1
#define AGREGADO 0
#define LLENO 2
#define GIGA (1024*1024*1024)
#include "nauty.h"
#include "profile_time.h"
struct nodo{
    graph *g;
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
        *comparacion=comparargraficas(g, pi->g, tgrafica);
        if (*comparacion==IGUAL)
            return ENCONTRADO;
        if (*comparacion==MAYOR)
            pi=pi->mayores;
        else if (*comparacion==MENOR)
            pi=pi->menores;
    }
    return NO_ENCONTRADO;
}
int encontrar_o_agregar(struct nodo **raiz, graph *g, int tgrafica){
    struct nodo *padre;
    int comparacion;
    if(buscargrafica(*raiz, g, tgrafica, &padre, &comparacion)==NO_ENCONTRADO){
        size_t k;
        struct nodo *N= (struct nodo *)malloc(sizeof(struct nodo));
        if (N==NULL)
            return 2;
        N->g=(graph *)malloc(sizeof(graph)*tgrafica);
        N->menores=NULL;
        N->mayores=NULL;
        for (k = 0; k < (size_t)tgrafica; ++k)
            (N)->g[k]=g[k];
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
        free(pi->g);
        free(pi);
    }
}
int buscarpar(int *tripletas, int tam, int a, int b){
    int i;
    b=(1<<b);
    b|=(1<<a);
    for (i=0; i<tam; i++)
        if ((tripletas[i]&b)==b)
            return ENCONTRADO;
    return NO_ENCONTRADO;
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
int buscarvertice(int *tripletas, int ifila, int a, int tlinea){
    int i, contador;
    for (i=0, contador=0; i<ifila; i++)
        if (tripletas[i]&(1<<a)){
            contador++;
            if (contador==tlinea)
                return ENCONTRADO;
        }
    return NO_ENCONTRADO;
}
int verificar(int *tripletas, int *vertices, int k, int  a, int tlinea){
    int i, ifila=k/(tlinea-1), icolumna=k%(tlinea-1);
    if(buscarvertice(tripletas, ifila, a, tlinea)==ENCONTRADO)
        return INVALIDO;
    if (icolumna==0){
        if (vertices[(ifila-1)*tlinea]==vertices[ifila*tlinea] && a<vertices[(ifila-1)*tlinea+1])
            return INVALIDO;
    }
    for (i=0; i<=icolumna; i++) {
        if(buscarpar(tripletas, ifila, vertices[ifila*tlinea+i], a)==1)
            return INVALIDO;
    }
    return VALIDO;
}
void actualizar(int *tripletas, int *vertices, int *S, int k, int  a, int tlinea){
    int icolumna=k%(tlinea-1), ifila=k/(tlinea-1);
    if (icolumna==tlinea-2) {
        int minimo, i;
        tripletas[ifila]=0;
        for (i=0; i<tlinea-1; ++i) {
            tripletas[ifila]|=(1<<vertices[ifila*tlinea+i]);
        }
        tripletas[ifila]|=(1<<a);
        vertices[(ifila+1)*tlinea-1]=a;
        minimo=vertices[ifila*tlinea];
        while (buscarvertice(tripletas, ifila+1, minimo, tlinea)==ENCONTRADO)
            minimo++;
        vertices[(ifila+1)*tlinea]=minimo;
        S[k-tlinea*(tlinea-1)+1]=minimo+1;
    }
    else{
        vertices[ifila*tlinea+icolumna+1]=a;
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
int calcularsistema(int nvertices, int tlinea, int salto, float limite) {
    struct rusage ru_begin, ru_end;
    struct timeval tv_elapsed;
    getrusage(RUSAGE_SELF, &ru_begin);//se comiensa a contar el tiempo
    struct nodo *csni[nvertices-tlinea];//Contenedor de soluciones no isomorfas
    int contador[nvertices-tlinea], aceptado[nvertices-tlinea], lab[2*nvertices], ptn[2*nvertices], orbits[2*nvertices], tripletas[nvertices], s[(nvertices-tlinea)*(tlinea-1)+1], vertices[tlinea*nvertices], i, k, a, tnoisomorfos;
    graph g[2*nvertices], canon[2*nvertices];
    //lab, optn, orbits, g y canon son nombres tomados del ejemplo en el manual de nauty y tienen el mismo rol
    for (i=0; i<nvertices-tlinea; i++){
        csni[i]=NULL;
        contador[i]=0;//llevara la cuanta de cuantos elementos hay en csni[i]
        aceptado[i]=TRUE;//marca para saber si usar si usar csni[i] o no
    }
    tnoisomorfos=0;//llevara la cuenta de los elementos en total de csni
    //calculo de cuantos elemntos de csni caben en la memoria asignada
    limite*=GIGA/(sizeof(struct nodo)+2*nvertices*sizeof(graph))*0.975;
    /******************************************************************
     arreglos para usar nauty
     ******************************************************************/
    nauty_check(WORDSIZE,1,MAXN,NAUTYVERSIONID);
    static DEFAULTOPTIONS_GRAPH(options);
    options.getcanon = TRUE;
    options.defaultptn=FALSE;
    statsblk stats;
    for (i=0; i<2*nvertices; i++){
        ptn[i]=1;
        lab[i]=i;
    }
    ptn[nvertices-1]=0;
    ptn[2*nvertices-1]=0;
    /******************************************************************
     arreglos para el backtraking
     ******************************************************************/
    for (i=0, k=0; i<tlinea; ++i) {
        tripletas[i]=0;
        tripletas[i]|=(1<<0);
        vertices[i*tlinea]=0;
        for (a=1; a<tlinea; ++a) {
            vertices[i*tlinea+a]=++k;
            tripletas[i]|=(1<<k);
        }
    }
    s[0]=tlinea;
    vertices[tlinea*tlinea]=1;
    k=tlinea*(tlinea-1);
    /******************************************************************
     backtraking
     ******************************************************************/
    while (k>tlinea*(tlinea-1)-1) {
        while (s[k-tlinea*(tlinea-1)]<nvertices) {
            a=s[k-tlinea*(tlinea-1)];
            s[k-tlinea*(tlinea-1)]++;
            if (verificar(tripletas, vertices, k, a, tlinea)==VALIDO) {
                actualizar(tripletas, vertices, s, k, a, tlinea);
                if (k%(tlinea-1)==tlinea-2 && aceptado[k/(tlinea-1)-tlinea]==TRUE) {
                    /******************************************************
                     Procesamiento de la solucion parcial
                     Primero se ve si es una solucion nueva
                     ******************************************************/
                    graficar(g, 2*nvertices, vertices, k/(tlinea-1)+1, nvertices, tlinea);
                    densenauty(g, lab, ptn, orbits, &options, &stats, 1, 2*nvertices, canon);
                    i=encontrar_o_agregar(&csni[k/(tlinea-1)-tlinea], canon, 2*nvertices);
                    if(i!=ENCONTRADO){
                        /**************************************************
                         Es una solucion nueva
                         Se cuenta y se ve si es una solucion total
                         **************************************************/
                        ++contador[k/(tlinea-1)-tlinea];
                        ++tnoisomorfos;
                        if (k==nvertices*(tlinea-1)-1 && contador[k/(tlinea-1)-tlinea]%salto==0) {
                            /******************************************************************
                             Es una solucion total. A imprimirla
                             ******************************************************************/
                            getrusage(RUSAGE_SELF, &ru_end);
                            timeval_subtract(&tv_elapsed, &ru_end.ru_utime, &ru_begin.ru_utime);
                            printf("\n%i soluciones encontradas. Tiempo %g ms.", contador[nvertices-tlinea-1], (tv_elapsed.tv_sec + (tv_elapsed.tv_usec/1000000.0))*1000.0);
                        }
                        else
                            ++k;
                        if (tnoisomorfos>limite||i==2) {
                            /******************************************************************
                             Se lleno la memoria hay que trabajar con menos datos
                             ******************************************************************/
                            a=0;
                            for (i=1; i<nvertices-tlinea; ++i)
                                if (aceptado[i]==TRUE && contador[a]<contador[i])
                                    a=i;
                            if(a==nvertices-tlinea-1){
                                printf("\nSe necesita mas memoria");
                                for (i=0; i<nvertices-tlinea; ++i)
                                    Borrar(csni[i]);
                                return 0;
                            }
                            else{
                                aceptado[a]=FALSE;
                                Borrar(csni[a]);
                                tnoisomorfos-=contador[a];
                                csni[a]=NULL;
                            }
                        }
                    }
                }
                else
                    ++k;
            }
        }
        k--;
    }
    /***************************************************
     Fin del backtraking hora de reportar los resultados
     ***************************************************/
    for (i=0; i<nvertices-tlinea; ++i)
        Borrar(csni[i]);
    printf("\nTotal: %i", contador[nvertices-tlinea-1]);
    getrusage(RUSAGE_SELF, &ru_end);
    timeval_subtract(&tv_elapsed, &ru_end.ru_utime, &ru_begin.ru_utime);
    printf("\nTiempo %g ms.\n", (tv_elapsed.tv_sec + (tv_elapsed.tv_usec/1000000.0))*1000.0);
    for (i=0; i<nvertices-tlinea; ++i){
        printf("\nPila %i llego al tamaño de %i elementos", i, contador[i]);
        if (aceptado[i]==FALSE)
            printf(" y se lleno la memoria");
    }
    return 0;
}
int main(int argc, const char * argv[]) {
    float limite;
    int nvertices, tlinea, salto;
    char continuar='s';
    while(continuar=='s'){
        /**********************************************************
         Se obtienen los datos
         **********************************************************/
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
        do{
            printf("¿Cada cuantas soluciones se imprime el progreso?\n");
            scanf("%d", &salto);
            if (salto<=0)
                printf("Se necesita un valor positivo\n");
        }while (salto<=0);
        /**********************************************************
         Se inicia el proceso
         **********************************************************/
        calcularsistema(nvertices, tlinea, salto, limite);
        printf("\n¿Desea continuar?\n");
        scanf("%s", &continuar);
    }
    return FALSE;
}