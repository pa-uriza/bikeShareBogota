# -*- coding: utf-8 -*-
"""
Created on Tue Apr  9 08:28:37 2019

@author: pa.uriza274
"""

import pyomo.environ as pyo
#from gurobipy import *
from os import chdir, getcwd
#Creo modelo
model = pyo.AbstractModel()

#Defino Conjuntos
                  
model.NOD = pyo.Set()                       # Conjunto de Nodos de Origen y Destino
model.NEst = pyo.Set()                      # Conjunto de Nodos de estaciones
model.NSink = pyo.Set() 

#model.N = pyo.Set()
model.N = model.NOD | model.NEst | model.NSink          # Conjunto de Nodos, unión de nodos OD y estaciones

model.OD = pyo.Set(within=model.NOD*model.NOD)          # Conjunto de Arcos, definidos en los parametros
model.A = pyo.Set(within=model.OD*model.N*model.N)      # Conjunto de Rutas     
        
  
#Conociendo los arcos posibles en el sistema 

#Define el subconjunto de nodos de salida para un nodo dado
def NodesOut_init(model, node): 
    retval = []
    for (i,j,k,l) in model.A: 
        if k == node:
            retval.append(l) 
    return retval
model.NodesOut = pyo.Set(model.N, initialize=NodesOut_init)
model.NEstOut = pyo.Set(model.NEst, initialize=NodesOut_init)


#Define el subconjunto de nodos de entrada para un nodo dado
def NodesIn_init(model, node): 
    retval = []
    for (i,j,k,l) in model.A: 
        if l == node:
            retval.append(k) 
    return retval
model.NodesIn = pyo.Set(model.N, initialize=NodesIn_init)
model.NEstIn = pyo.Set(model.NEst, initialize=NodesIn_init)

#Define el subconjunto de arcos que componen una ruta
#def ArcsInOD_init(model, no, nd): 
 #   retval = []
  #  for ni in model.NodesOut[no]:
   #     if ni in model.NSink: 
    #        if (ni,nd) in model.A:
    #            retval.append((no,ni))
     #           retval.append((ni,nd))
      #  else:
       #     if ni in model.NEst:
         #      for nj in model.NEstOut[ni]: 
          #         if nj in model.NEst:
           #            if (nj,nd) in model.A:
            #              retval.append((no,ni))
             #             retval.append((ni,nj))     
                #           retval.append((nj,nd))                     
   # return retval
#model.ArcsInOD = pyo.Set(model.OD,within=model.N*model.N, initialize=ArcsInOD_init)


model.Tmax = pyo.Param()
model.T = pyo.RangeSet(1,model.Tmax)                     # Conjunto de franjas de tiempo a analizar
model.W = pyo.Set()                     # Conjunto de tamaños de estaciones



#Defino parámetros

model.b = pyo.Param(model.OD,model.N,model.T,default = 0,mutable = True)    #Oferta o demanda en nodo ni para viajes con origen no  y destino nd, en la franja de tiempo t. ni,no,nd ∈ N,t ∈ T 
model.CBuild = pyo.Param(model.W)                    #Costo de construir una estación de tamaño w ∈ W
model.c = pyo.Param(model.N,model.N)                                #Costo del arco que va desde ni hasta nj. ni,nj ∈ N
model.Q = pyo.Param (model.W)                       #Capacidad de una estación de tamaño w ∈ W
model.Budget = pyo.Param()                           #Presupuesto para construir estaciones de bicicleta
model.CMantainance = pyo.Param()                          #Costo diario de tener una bicicleta
model.CAcquire = pyo.Param()                            #Costo de adquirir una bicicleta




#Defino las variables de decisión 

#model.x = pyo.Var(model.OD, model.ArcsInOD, model.T, within=pyo.NonNegativeReals)                 #Flujo del arco (ni,nj)  para pasajeros con origen no  y destino nd,en la franja de tiempo t. ∀ ni,nj,no,nd ∈ N, t ∈ T.
model.x = pyo.Var(model.A, model.T, within=pyo.NonNegativeReals)  
model.y = pyo.Var(model.NEst, model.W, within=pyo.Binary)                                         # 1: si en el nodo ni  ∈ CN(est)  se decide construir una estación de tamaño w ∈ W. 0 dlc
model.z = pyo.Var(model.NEst,model.T, within=pyo.NonNegativeReals)                              # Inventario en la estación ni  al inicio de la franja de tiempo t.  ∀  ni ∈ NC(est),t ∈ T  




#Función objetivo

#minimiza el costo de uso y de operación del sistema

def obj_rule(model):
    return (sum(model.x[no,nd,ni,nj,t]*model.c[ni,nj] for (no,nd,ni,nj) in model.A for t in model.T )+sum (model.z[ni,1]*model.CMantainance for ni in model.NEst)) #Preguntar notación t=1
model.obj = pyo.Objective(rule=obj_rule)

#Restricciones

# 1. Debo cumplir con todas las demandas entre puntos de origen y destino:
def cumplir_demandas_rule(model,no,nd,ni,t):
    return ((sum(model.x[no,nd,ni,nj,t] for nj in model.NodesOut[ni] if (no,nd,ni,nj) in  model.A)  - sum(model.x[no,nd,nj,ni,t] for nj in model.NodesIn[ni] if (no,nd,nj,ni) in  model.A))==model.b[no,nd,ni,t])
model.cumplir_demandas_rule = pyo.Constraint(model.OD, model.N, model.T, rule= cumplir_demandas_rule)


#2. En caso de que decida abrir una estación sólo le puedo asignar un tamaño
def un_tamaño_por_estacion_rule(model, ni):
    return sum(model.y[ni,w] for w in model.W) <= 1  
model.one_size_per_station_rule = pyo.Constraint(model.NEst, rule=un_tamaño_por_estacion_rule)



#3. Balanceo de inventario de bicicletas
def balanceo_bicicletas_rule(model,ni,t):
    if t < model.Tmax:
        return ((sum(model.x[no,nd,nj,ni,t] for (no,nd) in model.OD for nj in model.NEstIn[ni] if (no,nd,nj,ni) in model.A ) - sum(model.x[no,nd,ni,nj,t] for (no,nd) in model.OD for nj in model.NEstOut[ni] if (no,nd,ni,nj) in model.A ) + model.z[ni,t])== model.z[ni,(t+1)])
    else:
        return pyo.Constraint.Skip
model.balanceo_bicicletas_rule = pyo.Constraint(model.NEst, model.T, rule=balanceo_bicicletas_rule)

#4. Para cada estación, el inventario no puede superar la capacidad en ninguna franja horaria
def capcidad_inventario_rule(model,ni,t):
    return (model.z[ni,t]<=sum(model.y[ni,w]*model.Q[w] for w in model.W))
model.capcidad_inventario_rule = pyo.Constraint(model.NEst, model.T, rule=capcidad_inventario_rule)

#5.Para cada estación, el flujo de salida de una estación hacia una estación no puede superar a la flota de bicicletas disponible en ese momento
def flujo_salida_inventario_rule(model,ni,t):
    return (sum(model.x[no,nd,ni,nj,t] for (no,nd) in model.OD for nj in model.NEstOut[ni] if (no,nd,ni,nj) in model.A) <= model.z[ni,t])
model.flujo_salida_inventario_rule = pyo.Constraint(model.NEst, model.T, rule= flujo_salida_inventario_rule)

#9.Presupuesto
def presupuesto_rule(model):
    return (sum (model.y[ni,w]* model.CBuild[w] for ni in model.NEst for w in model.W) + model.CAcquire* sum (model.z[ni,1] for ni in model.NEst)<=model.Budget)
model.presupuesto = pyo.Constraint(rule=presupuesto_rule)


# chdir("/Users/lukasbogota/Desktop/PG2/Pyomo")
instance = model.create_instance("ToyInstance.dat")
##Resolver el modelo
opt = pyo.SolverFactory('gurobi')

#instance.presupuesto.display()

# Optimize
results = opt.solve(instance)

# Write the output
results.write(num=1)

           
for v in instance.component_objects(pyo.Var,active=True):
    varobject = getattr(instance, str(v))
    for index in varobject:
        if varobject[index].value > 0:
            print ("   ", index, varobject[index].value)