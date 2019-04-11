# -*- coding: utf-8 -*-
"""
Created on Tue Apr  9 08:28:37 2019

@author: pa.uriza274
"""

import pyomo.environ as pyo
import pandas as pd
from os import chdir, getcwd

#Read data
# chdir("/Users/lukasbogota/Desktop/PG2/Pyomo")
path='ToyInstance.xlsx'
NOD = pd.read_excel(path, sheetname='NOD', index_col = [0])
NEst = pd.read_excel(path, sheetname='NEst', index_col = [0])
NSink = pd.read_excel(path, sheetname='NSink', index_col = [0])
A = pd.read_excel(path, sheetname='A', index_col = [0,1,2,3])
Stations = pd.read_excel(path, sheetname='Stations', index_col = [0])
b = pd.read_excel(path, sheetname='b', index_col = [0,1,2,3])
c = pd.read_excel(path, sheetname='c', index_col = [0,1])
Params = pd.read_excel(path, sheetname='Params', header=0 , index_col = [0])

#Define solver
solver = pyo.SolverFactory('gurobi')

#Creo modelo
model = pyo.ConcreteModel()

#Defino Conjuntos
                  
model.NOD = pyo.Set(initialize = NOD.index.tolist())           # Set of origin and destination nodes
model.NEst = pyo.Set(initialize = NEst.index.tolist())         # Set of station nodes
model.NSink = pyo.Set(initialize = NSink.index.tolist())       # Set of sink nodes
model.A = pyo.Set(initialize = A.index.tolist())               # Set of Arcs    

model.N = model.NOD | model.NEst | model.NSink          # Set of all nodes
model.OD = pyo.Set(initialize = set([(no,nd) for (no,nd,ni,nj) in model.A]))        # Set of OD pairs

        
  
#Conociendo los arcos posibles en el sistema 

#Define el subconjunto de nodos de salida para un nodo dado
def NodesOut_init(model, node): 
    retval = []
    for (i,j,k,l) in model.A: 
        if k == node:
            retval.append(l) 
    return retval
model.NodesOut = pyo.Set(model.N, initialize=NodesOut_init)

#Define el subconjunto de nodos de entrada para un nodo dado
def NodesIn_init(model, node): 
    retval = []
    for (i,j,k,l) in model.A: 
        if l == node:
            retval.append(k) 
    return retval
model.NodesIn = pyo.Set(model.N, initialize=NodesIn_init)

#Define el subconjunto de nodos de estación de salida para un nodo de estación dado
def StationNodesOut_init(model, node): 
    retval = []
    for (i,j,k,l) in model.A: 
        if k == node and l in model.NEst:
            retval.append(l) 
    return retval
model.NEstOut = pyo.Set(model.NEst, initialize=StationNodesOut_init)

#Define el subconjunto de nodos de entrada para un nodo dado
def StationNodesIn_init(model, node): 
    retval = []
    for (i,j,k,l) in model.A: 
        if l == node and k in model.NEst:
            retval.append(k) 
    return retval
model.NEstIn = pyo.Set(model.NEst, initialize=StationNodesIn_init)



model.Tmax = pyo.Param(initialize = Params.to_dict()['Value']['Tmax'])
model.T = pyo.RangeSet(1,model.Tmax)                     # Conjunto de franjas de tiempo a analizar
model.W = pyo.Set(initialize = Stations.index.tolist())                     # Conjunto de tamaños de estaciones



#Defino parámetros

model.b = pyo.Param(model.OD,model.N,model.T,default = 0,mutable = True, initialize = b.to_dict()['b'])    #Oferta o demanda en nodo ni para viajes con origen no  y destino nd, en la franja de tiempo t. ni,no,nd ∈ N,t ∈ T 
model.CBuild = pyo.Param(model.W,initialize = Stations.to_dict()['CBuild'])                    #Costo de construir una estación de tamaño w ∈ W
model.c = pyo.Param(model.N,model.N,initialize = c.to_dict()['c'])                                #Costo del arco que va desde ni hasta nj. ni,nj ∈ N
model.Q = pyo.Param (model.W,initialize = Stations.to_dict()['Q'])                       #Capacidad de una estación de tamaño w ∈ W
model.Budget = pyo.Param(initialize = Params.to_dict()['Value']['Budget'])                           #Presupuesto para construir estaciones de bicicleta
model.CMantainance = pyo.Param(initialize = Params.to_dict()['Value']['CMantainance'])                          #Costo diario de tener una bicicleta
model.CAcquire = pyo.Param(initialize = Params.to_dict()['Value']['CAcquire'])                            #Costo de adquirir una bicicleta




#Decision variables
            
model.x = pyo.Var(model.A, model.T, within=pyo.NonNegativeReals)        #Flujo del arco (ni,nj)  para pasajeros con origen no  y destino nd,en la franja de tiempo t. ∀ ni,nj,no,nd ∈ N, t ∈ T.
model.y = pyo.Var(model.NEst, model.W, within=pyo.Binary)               # 1: si en el nodo ni  ∈ CN(est)  se decide construir una estación de tamaño w ∈ W. 0 dlc
model.z = pyo.Var(model.NEst,model.T, within=pyo.NonNegativeReals)      # Inventario en la estación ni  al inicio de la franja de tiempo t.  ∀  ni ∈ NC(est),t ∈ T  



#Objective function

#minimiza el costo de uso y de operación del sistema

def obj_rule(model):
    return (sum(model.x[no,nd,ni,nj,t]*model.c[ni,nj] for (no,nd,ni,nj) in model.A for t in model.T )+sum (model.z[ni,1]*model.CMantainance for ni in model.NEst)) #Preguntar notación t=1
model.obj = pyo.Objective(rule=obj_rule)

#Constraints

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
        return (sum(model.x[no,nd,nj,ni,t] for (no,nd) in model.OD for nj in model.NEstIn[ni] if (no,nd,nj,ni) in model.A ) - sum(model.x[no,nd,ni,nj,t] for (no,nd) in model.OD for nj in model.NEstOut[ni] if (no,nd,ni,nj) in model.A ) + model.z[ni,t])== model.z[ni,(t+1)]
    else:
        return (sum(model.x[no,nd,nj,ni,t] for (no,nd) in model.OD for nj in model.NEstIn[ni] if (no,nd,nj,ni) in model.A ) - sum(model.x[no,nd,ni,nj,t] for (no,nd) in model.OD for nj in model.NEstOut[ni] if (no,nd,ni,nj) in model.A ) + model.z[ni,t])<=sum(model.y[ni,w]*model.Q[w] for w in model.W)
model.balanceo_bicicletas_rule = pyo.Constraint(model.NEst, model.T, rule=balanceo_bicicletas_rule)

#4. Para cada estación, el inventario no puede superar la capacidad en ninguna franja horaria
def capacidad_inventario_rule(model,ni,t):
    return model.z[ni,t]<=sum(model.y[ni,w]*model.Q[w] for w in model.W)
model.capacidad_inventario_rule = pyo.Constraint(model.NEst, model.T, rule=capacidad_inventario_rule)

#5.Para cada estación, el flujo de salida de una estación hacia una estación no puede superar a la flota de bicicletas disponible en ese momento
def flujo_salida_inventario_rule(model,ni,t):
    return sum(model.x[no,nd,ni,nj,t] for (no,nd) in model.OD for nj in model.NEstOut[ni] if (no,nd,ni,nj) in model.A) <= model.z[ni,t]
model.flujo_salida_inventario_rule = pyo.Constraint(model.NEst, model.T, rule= flujo_salida_inventario_rule)

#9.Presupuesto
def presupuesto_rule(model):
    return sum (model.y[ni,w]* model.CBuild[w] for ni in model.NEst for w in model.W) + model.CAcquire* sum (model.z[ni,1] for ni in model.NEst)<=model.Budget
model.presupuesto = pyo.Constraint(rule=presupuesto_rule)

##Solve

# Optimize
results = solver.solve(model)

# Write the output
results.write(num=0)

           
for v in model.component_objects(pyo.Var,active=True):
    varobject = getattr(model, str(v))
    for index in varobject:
        if varobject[index].value > 0:
            print ("   ", index, varobject[index].value)
            