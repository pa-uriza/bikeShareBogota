#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Mar 25 17:49:26 2019

@author: lukasbogota
"""


import pyomo.environ as pyo
#from gurobipy import *
from os import chdir, getcwd
#Creo modelo
model = pyo.AbstractModel()

#Defino Conjuntos
                  
model.NOD = pyo.Set()                       # Conjunto de Nodos de Origen y Destino
model.NEst = pyo.Set()                      # Conjunto de Nodos de estaciones

#model.N = pyo.Set()
model.N = model.NOD | model.NEst        # Conjunto de Nodos, unión de nodos OD y estaciones


model.A = pyo.Set(within=model.N*model.N)              # Conjunto de Arcos, definidos en los parametros
model.OD = pyo.Set(within=model.NOD*model.NOD)         # Conjunto de Rutas 

#Conociendo los arcos posibles en el sistema 

#Define el subconjunto de nodos de salida para un nodo dado
def NodesOut_init(model, node): 
    retval = []
    for (i,j) in model.A: 
        if i == node:
            retval.append(j) 
    return retval
model.NodesOut = pyo.Set(model.N, initialize=NodesOut_init)
model.NEstOut = pyo.Set(model.NEst, initialize=NodesOut_init)


#Define el subconjunto de nodos de entrada para un nodo dado
def NodesIn_init(model, node): 
    retval = []
    for (i,j) in model.A: 
        if j == node:
            retval.append(i) 
    return retval
model.NodesIn = pyo.Set(model.N, initialize=NodesIn_init)
model.NEstIn = pyo.Set(model.NEst, initialize=NodesIn_init)

#Define el subconjunto de arcos que componen una ruta
def ArcsInRoute_init(model, no, nd): 
    retval = []
    for ni in model.NodesOut[no]:
        if ni == "E0":
            if (ni,nd) in model.A:
                retval.append((no,ni))
                retval.append((ni,nd))
        else:
            if ni in model.NEst:
               for nj in model.NEstOut[ni]: 
                   if nj in model.NEst:
                       if (nj,nd) in model.A:
                          retval.append((no,ni))
                          retval.append((ni,nj))     
                          retval.append((nj,nd))                     
    return retval
model.ArcsInRoute = pyo.Set(model.OD,within=model.N*model.N, initialize=ArcsInRoute_init)


model.Tmax = pyo.Param()
model.T = pyo.RangeSet(1,model.Tmax)                     # Conjunto de franjas de tiempo a analizar
model.W = pyo.Set()                     # Conjunto de tamaños de estaciones



#Defino parámetros

model.b = pyo.Param(model.OD,model.N,model.T,default = 0,mutable = True)    #Oferta o demanda en nodo ni para viajes con origen no  y destino nd, en la franja de tiempo t. ni,no,nd ∈ N,t ∈ T 
model.Cconstruccion = pyo.Param(model.W)                    #Costo de construir una estación de tamaño w ∈ W
model.c = pyo.Param(model.A)                                #Costo del arco que va desde ni hasta nj. ni,nj ∈ N
model.capacidad = pyo.Param (model.W)                       #Capacidad de una estación de tamaño w ∈ W
model.Pconstruccion = pyo.Param()                           #Presupuesto para construir estaciones de bicicleta
model.Cmantenimiento = pyo.Param()                          #Costo diario de tener una bicicleta
model.Cadquisicion = pyo.Param()                            #Costo de adquirir una bicicleta


#Defino las variables de desición 

# model.x = pyo.Var(model.OD, model.ArcsInRoute[model.OD], model.T, within=pyo.NonNegativeReals)                 #Flujo del arco (ni,nj)  para pasajeros con origen no  y destino nd,en la franja de tiempo t. ∀ ni,nj,no,nd ∈ N, t ∈ T.
model.x = pyo.Var(model.OD, model.A, model.T, within=pyo.NonNegativeReals)  
model.y = pyo.Var(model.NEst, model.W, within=pyo.Binary)                                         # 1: si en el nodo ni  ∈ CN(est)  se decide construir una estación de tamaño w ∈ W. 0 dlc
model.z = pyo.Var(model.NEst,model.T, within=pyo.NonNegativeReals)                              # Inventario en la estación ni  al inicio de la franja de tiempo t.  ∀  ni ∈ NC(est),t ∈ T  



#Función objetivo

#minimiza el costo de uso y de operación del sistema

def obj_rule(model):
    return (sum(model.x[od,a,t]*model.c[a] for od in model.OD for a in model.ArcsInRoute[od] for t in model.T  )+sum (model.z[ni,1]*model.Cmantenimiento for ni in model.NEst)) #Preguntar notación t=1
model.obj = pyo.Objective(rule=obj_rule)
#Preguntar recorridos sobre arcos

#Restricciones

# 1. Debo cumplir con todas las demandas entre puntos de origen y destino:
def cumplir_demandas_rule(model,no,nd,ni,t):
    return ((sum(model.x[no,nd,ni,nj,t] for nj in model.NodesOut[ni] if (ni,nj) in model.ArcsInRoute[no,nd])  - sum(model.x[no,nd,nj,ni,t] for nj in model.NodesIn[ni] if (ni,nj) in model.ArcsInRoute[no,nd]))==model.b[no,nd,ni,t])
model.cumplir_demandas_rule = pyo.Constraint(model.OD, model.N, model.T, rule= cumplir_demandas_rule)


#2. En caso de que decida abrir una estación sólo le puedo asignar un tamaño
def un_tamaño_por_estacion_rule(model, ni):
        if ni == "E0":
            return sum(model.y[ni,w] for w in model.W) == 0
        else:
            return sum(model.y[ni,w] for w in model.W) <= 1  
model.one_size_per_station_rule = pyo.Constraint(model.NEst, rule=un_tamaño_por_estacion_rule)



#3. Balanceo de inventario de bicicletas
def balanceo_bicicletas_rule(model,ni,t):
    if ni == "E0":
        return (model.z[ni,t]==0)
    else:
        if t < model.Tmax:
            return ((sum(model.x[od,nj,ni,t] for od in model.OD for nj in model.NEstIn[ni]if (ni,nj) in model.ArcsInRoute[od] ) - sum(model.x[od,ni,nj,t] for od in model.OD for nj in model.NEstOut[ni] if (ni,nj) in model.ArcsInRoute[od] ) + model.z[ni,t])== model.z[ni,(t+1)])
        else:
            return pyo.Constraint.Skip
model.balanceo_bicicletas_rule = pyo.Constraint(model.NEst, model.T, rule=balanceo_bicicletas_rule) #PREGUNTAR!!

#4. Para cada estación, el inventario no puede superar la capacidad en ninguna franja horaria
def capcidad_inventario_rule(model,ni,t):
    if ni == "E0":
         return pyo.Constraint.Skip
    else:
        return (model.z[ni,t]<=sum(model.y[ni,w]*model.capacidad[w] for w in model.W))
model.capcidad_inventario_rule = pyo.Constraint(model.NEst, model.T, rule=capcidad_inventario_rule)

#5.Para cada estación, el flujo de salida de una estación hacia una estación no puede superar a la flota de bicicletas disponible en ese momento
def flujo_salida_inventario_rule(model,ni,t):
     if ni == "E0":
         return pyo.Constraint.Skip
     else:
         return (sum(model.x[od,ni,nj,t] for od in model.OD for nj in model.NEstOut if (ni,nj) in model.ArcsInRoute[od]) <= model.z[ni,t])
model.flujo_salida_inventario_rule = pyo.Constraint(model.NEst, model.T, rule= flujo_salida_inventario_rule)

#9.Presupuesto
def presupuesto_rule(model):
    return (sum (model.y[ni,w]* model.Cconstruccion[w] for ni in model.NEst for w in model.W) + model.Cadquisicion* sum (model.z[ni,1] for ni in model.NEst)<=model.Pconstruccion)
model.presupuesto = pyo.Constraint(rule=presupuesto_rule)


#Cargo los datos al modelo, sólo sirve si no corro la función objetivo ni las restricciones. 
getcwd()
chdir("/Users/lukasbogota/Desktop/PG2/Pyomo")
getcwd()
instance = model.create_instance("PG2Pyomo.dat")
#Resolver el modelo
opt = pyo.SolverFactory('gurobi')
opt.solve(instance)

instance.presupuesto.display()



for v in instance.component_objects(pyo.Var, active=True):
    print("Variable",v) # doctest: +SKIP
    for index in v:
            print (" ",index, pyo.value(v[index])) # doctest: +SKIP

#for v in instance.component_objects(Var):
#    varobject = getattr(instance, str(v))
#    for index in varobject:
#         print ("   ", index, varobject[index].value)
#        if varobject[index].value > 0:
#            print ("   ", index, varobject[index].value)


         
         