import numpy as np
import numpy.matlib as matlib

import shutil,os,glob
import logging
import xarray as xr

logging.basicConfig(level=logging.DEBUG)

_log = logging.getLogger(__name__)

# toplevel parameters:

H = 2000  # water depth meters
g = 9.81  # m/s^2
f0 = 1.0e-4  # rad/s

# tidal forcing:
u0 = 0.1   # m/s

# Domain size:

nx = 40
ny = 20
nz = 20

dx = 10000  # m
dy = 20000  # m

z = np.linspace(0, H, nz + 1)
dz = (z[1:] - z[:-1]) / 2
z = z[:-1] + dz
with open("delZ.bin", "wb") as f:
	dz.tofile(f)

x = np.arange(nx+1) * dx
dx = np.diff(x)
x = x[:-1] + dx / 2
with open("delX.bin", "wb") as f:
  dx.tofile(f)

y = np.arange(ny + 1) * dy
dy = np.diff(y)
y = y[:-1] + dy / 2
with open("delY.bin", "wb") as f:
  dy.tofile(f)

# temperature field (linear, top down)

T0 = 16 - z / H * 12
with open("T0.bin", "wb") as f:
    T0.tofile(f)

# Make the topography

d = - np.ones((ny, nx)) * H
with open("topog.bin", "wb") as f:
  d.tofile(f)

#### make the tidal forcing for each boundary.
# this tidal wave is propagating from east to west
# and has uniform velocity across the channel.

c = np.sqrt(g * H)
om = 2 * np.pi / 12.4 / 3600.
k = np.sqrt(om**2 - f0**2) / c

u = u0 * np.exp(-(x - x[-1]) * k * 1j)
v = -1j * f0 / om * u

# MITGCM wants the amplitude at each grid cell of the boundary
# and a time offset.
# dt = -phase  / om

# East

amp = np.abs(u[-1]) * np.ones((nz, ny))
dt = - np.angle(u[-1]) / om * np.ones((nz, ny))
with open("UEamp.bin", "wb") as f:
    amp.tofile(f)
with open("UEdt.bin", "wb") as f:
    dt.tofile(f)

amp = np.abs(v[-1]) * np.ones((nz, ny))
dt = - np.angle(v[-1]) / om * np.ones((nz, ny))
with open("VEamp.bin", "wb") as f:
    amp.tofile(f)
with open("VEdt.bin", "wb") as f:
    dt.tofile(f)

# West

amp = np.abs(u[0]) * np.ones((nz, ny))
dt = - np.angle(u[0]) / om * np.ones((nz, ny))
with open("UWamp.bin", "wb") as f:
    amp.tofile(f)
with open("UWdt.bin", "wb") as f:
    dt.tofile(f)

amp = np.abs(v[0]) * np.ones((nz, ny))
dt = - np.angle(v[0]) / om * np.ones((nz, ny))
with open("VWamp.bin", "wb") as f:
    amp.tofile(f)
with open("VWdt.bin", "wb") as f:
    dt.tofile(f)

# North and south (same)

amp = matlib.repmat(np.abs(u), nz, 1)
dt = -matlib.repmat(np.angle(u), nz, 1) / om
with open("UNamp.bin", "wb") as f:
    amp.tofile(f)
with open("UNdt.bin", "wb") as f:
    dt.tofile(f)
with open("USamp.bin", "wb") as f:
    amp.tofile(f)
with open("USdt.bin", "wb") as f:
    dt.tofile(f)

amp = matlib.repmat(np.abs(v), nz, 1)
dt = -matlib.repmat(np.angle(v), nz, 1) / om
with open("VNamp.bin", "wb") as f:
    amp.tofile(f)
with open("VNdt.bin", "wb") as f:
    dt.tofile(f)
with open("VSamp.bin", "wb") as f:
    amp.tofile(f)
with open("VSdt.bin", "wb") as f:
    dt.tofile(f)
