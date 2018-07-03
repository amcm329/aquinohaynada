
# coding: utf-8

# In[6]:


import math


# In[16]:


x='Cuando cuentes cuentos cuenta cuantos cuentos cuentas'
len("".join(x.split()))


# In[23]:


from collections import Counter


# In[22]:


c = Counter(x)
print (c)


# In[25]:


{i:x.count(i) for i in x} 


# In[3]:


x='Cuando cuentes cuentos cuenta cuantos cuentos cuentas'

d={}

for y in x:
    if y not in d:
        d[y]=1
    else:
        d[y]=d[y]+1 
print (d)

