//
//  main.c
//  SRP
//
//  Created by Víctor Andrés Hernández Patiño on 21/08/15.
//  Copyright (c) 2015 Víctor Andrés Hernández Patiño. All rights reserved.
//
#include <stdio.h>
void imprimirlineas(int nvertices, int tlinea, int *lineas, int tam){
    int i, j, nencontrados;
    for (i=0; i<tam; ++i) {
        printf("(");
        for (j=0, nencontrados=0; j<nvertices && nencontrados<tlinea; ++j){
            if (lineas[i]&(1<<j)){
                ++nencontrados;
                printf("%i", j);
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
//sistema resoluble por pasos
void SRP(int nvertices, int tlinea, int salto){
    int lineas[nvertices], vertices[nvertices*tlinea];
    int i, k, a;
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
    lineas[tlinea]=1;
}
int main(int argc, const char * argv[]) {
    float limite;
    int nvertices, tlinea, salto;
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
        do{
            printf("¿Cada cuantas soluciones se imprime el progreso?\n");
            scanf("%d", &salto);
            if (salto<=0)
                printf("Se necesita un valor positivo\n");
        }while (salto<=0);
        /************************************************
                Se inicia el proceso
         ************************************************/
        SRP(nvertices, tlinea, salto);
        printf("\n¿Desea continuar?\n");
        scanf("%s", &continuar);
    }
    return 0;
}
