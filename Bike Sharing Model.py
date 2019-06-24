# -*- coding: utf-8 -*-
"""
Created on Tue Apr  9 08:28:37 2019
@author: pa.uriza274
"""


import pyomo.environ as pyo
import pandas as pd
from os import chdir, getcwd

#%% Read data

chdir("/Users/lukasbogota/Desktop/PG2/Pyomo/Beneficio Social/Definitivo/UCS/Estaciones")
path='ParametrosBS_UCS_Estaciones.xlsx'
NEst = pd.read_excel(path, sheetname='NEst', index_col = [0])           
NSink = pd.read_excel(path, sheetname='NSink', index_col = [0])         
A = pd.read_excel(path, sheetname='A', index_col = [0,1,2,3])
Stations = pd.read_excel(path, sheetname='Stations', index_col = [0])
b = pd.read_excel(path, sheetname='b', index_col = [0,1,2,3])
c = pd.read_excel(path, sheetname='c', index_col = [0,1])
Params = pd.read_excel(path, sheetname='Params', header=0 , index_col = [0])

#%% Define solver
solver = pyo.SolverFactory('gurobi')

# %%Creo modelo
model = pyo.ConcreteModel(name = "Bike Sharing")

# Sets
A 
model.A = pyo.Set(initialize = A.index.tolist())                                    # Set of Arcs 
model.OD = pyo.Set(initialize = set([(no,nd) for (no,nd,ni,nj) in model.A]))        # Set of OD pairs                   
model.NEst = pyo.Set(initialize = NEst.index.tolist())                              # Set of station nodes
model.NSink = pyo.Set(initialize = NSink.index.tolist())                            # Set of sink nodes

NOD = []
for (no,nd) in model.OD: NOD.extend([no,nd])
model.NOD = pyo.Set(initialize = set(NOD))              # Set of origin and destination nodes

model.N = model.NOD | model.NEst | model.NSink          # Set of all nodes


# Defines the subset of output nodes for a given node
def NodesOut_init(model, node): 
    retval = []
    for (i,j,k,l) in model.A: 
        if k == node:
            retval.append(l) 
    return retval
model.NodesOut = pyo.Set(model.N, initialize=NodesOut_init)

# Defines the subset of input nodes for a given node
def NodesIn_init(model, node): 
    retval = []
    for (i,j,k,l) in model.A: 
        if l == node:
            retval.append(k) 
    return retval
model.NodesIn = pyo.Set(model.N, initialize=NodesIn_init)


# Defines the subset of output station nodes for a given station node
def StationNodesOut_init(model, node): 
    retval = []
    for (i,j,k,l) in model.A: 
        if k == node and l in model.NEst:
            retval.append(l) 
    return retval
model.NEstOut = pyo.Set(model.NEst, initialize=StationNodesOut_init)


# Defines the subset of input station nodes for a given station node
def StationNodesIn_init(model, node): 
    retval = []
    for (i,j,k,l) in model.A: 
        if l == node and k in model.NEst:
            retval.append(k) 
    return retval
model.NEstIn = pyo.Set(model.NEst, initialize=StationNodesIn_init)



model.Tmax = pyo.Param(initialize = Params.to_dict()['Value']['Tmax'])
model.T = pyo.RangeSet(1,model.Tmax)                                             # Set of time bands to be analyzed
model.W = pyo.Set(initialize = Stations.index.tolist())                          # Set of station sizes



# Parameters

model.b = pyo.Param(model.OD,model.N,model.T,default = 0,mutable = True, initialize = b.to_dict()['b'])     # Offer or demand in node ni for trips with origin no and destination nd, in time slot t. no, no, nd ∈ N, t ∈ T 
model.CBuild = pyo.Param(model.W,initialize = Stations.to_dict()['CBuild'])                                 # Cost to build a station of size w ∈ W
model.c = pyo.Param(model.N,model.N,default = 0,initialize = c.to_dict()['c'])                              # Cost of the arch that goes from ni to nj. ni, nj ∈ N
model.Q = pyo.Param (model.W,initialize = Stations.to_dict()['Q'])                                          # Capacity of a station of size w ∈ W
model.Budget = pyo.Param(initialize = Params.to_dict()['Value']['Budget'])                                  # Budget to build bicycle stations
model.CMantainance = pyo.Param(initialize = Params.to_dict()['Value']['CMantainance'])                      # Daily cost of having a bicycle
model.CAcquire = pyo.Param(initialize = Params.to_dict()['Value']['CAcquire'])                              # Cost of acquiring a bicycle
model.OpBudget = pyo.Param(initialize = Params.to_dict()['Value']['OpBudget'])   



#Decision variables
            
model.x = pyo.Var(model.A, model.T, within=pyo.NonNegativeReals)        # Arc flow (ni, nj) for passengers with origin no and destination nd, in time slot t. ∀ ni, nj, no, nd ∈ N, t ∈ T.
model.y = pyo.Var(model.NEst, model.W, within=pyo.Binary)               # 1: if in the node or ∈ CN (est) it is decided to build a station of size w ∈ W. 0 ioc
model.z = pyo.Var(model.NEst,model.T, within=pyo.NonNegativeIntegers)      # Inventory at the station ni at the beginning of the time slot t. ∀ ni ∈ NC (est), t ∈ T



#Objective function

# minimizes the cost of use and operation of the system

def obj_rule(model):
    return (sum(model.x[no,nd,ni,nj,t]*model.c[ni,nj] for (no,nd,ni,nj) in model.A for t in model.T )) 
model.obj = pyo.Objective(rule=obj_rule)

#Constraints

# 1. It must meet all the demands between points of origin and destination:
def meet_demands_rule(model,no,nd,ni,t):
    return ((sum(model.x[no,nd,ni,nj,t] for nj in model.NodesOut[ni] if (no,nd,ni,nj) in  model.A)  - sum(model.x[no,nd,nj,ni,t] for nj in model.NodesIn[ni] if (no,nd,nj,ni) in  model.A))==model.b[no,nd,ni,t])
model.meet_demands_rule = pyo.Constraint(model.OD, model.N, model.T, rule= meet_demands_rule)


#2. In case it is decided to open a station, it can only have one size
def one_size_per_station_rule(model, ni):
    return sum(model.y[ni,w] for w in model.W) <= 1  
model.one_size_per_station_rule = pyo.Constraint(model.NEst, rule=one_size_per_station_rule)


#3. bicycle inventory balance
def bicycle_inventory_balance_rule(model,ni,t):
    if t < model.Tmax:
        return (sum(model.x[no,nd,nj,ni,t] for (no,nd) in model.OD for nj in model.NEstIn[ni] if (no,nd,nj,ni) in model.A ) - sum(model.x[no,nd,ni,nj,t] for (no,nd) in model.OD for nj in model.NEstOut[ni] if (no,nd,ni,nj) in model.A ) + model.z[ni,t])== model.z[ni,(t+1)]
    else:
        return (sum(model.x[no,nd,nj,ni,t] for (no,nd) in model.OD for nj in model.NEstIn[ni] if (no,nd,nj,ni) in model.A ) - sum(model.x[no,nd,ni,nj,t] for (no,nd) in model.OD for nj in model.NEstOut[ni] if (no,nd,ni,nj) in model.A ) + model.z[ni,t])<=sum(model.y[ni,w]*model.Q[w] for w in model.W)
model.bicycle_inventory_balance_rule = pyo.Constraint(model.NEst, model.T, rule=bicycle_inventory_balance_rule)


#4. For each station, the bicycle inventory can not exceed capacity in any time slot
def inventory_capacity_rule(model,ni,t):
    return model.z[ni,t]<=sum(model.y[ni,w]*model.Q[w] for w in model.W)
model.inventory_capacity_rule = pyo.Constraint(model.NEst, model.T, rule=inventory_capacity_rule)


#5. For each station, the outflow of a station to a station can not exceed the fleet of bicycles available at that time
def station_outflow_rule(model,ni,t):
    return sum(model.x[no,nd,ni,nj,t] for (no,nd) in model.OD for nj in model.NEstOut[ni] if (no,nd,ni,nj) in model.A) <= model.z[ni,t]
model.station_outflow_rule = pyo.Constraint(model.NEst, model.T, rule= station_outflow_rule)


#6. Budget construction
def budget_construction_rule(model):
    return sum (model.y[ni,w]* model.CBuild[w] for ni in model.NEst for w in model.W) + model.CAcquire* sum (model.z[ni,1] for ni in model.NEst)<=model.Budget
model.budget_construction_rule = pyo.Constraint(rule=budget_construction_rule)

#7. Operational Budget
def operational_budget_rule(model):
    return sum (model.z[ni,1]*model.CMantainance for ni in model.NEst) <= model.OpBudget
model.operational_budget_rule = pyo.Constraint(rule=operational_budget_rule)

#%%Solve
# Optimize
results = solver.solve(model)

# Write the output

# model.display()
           

#for v in model.component_objects(pyo.Var,active=True):
#    varobject = getattr(model, str(v))
#    for index in varobject:
#        if varobject[index].value > 0:
#            print ("   ", index, varobject[index].value)


#%%Test





for k in model.y.keys():
    if abs(model.y[k].value)>0:
        print (model.y[k].getname(), model.y[k].value)
        
for k in model.z.keys():
    if abs(model.z[k].value)>0:
        print (model.z[k].getname(), model.z[k].value)
        

        
 #%%Test
results.write(num=1)
 #%%Test
for k in model.x.keys():
    if abs(model.x[k].value)>0:
        
            print (model.x[k].getname(), model.x[k].value)
