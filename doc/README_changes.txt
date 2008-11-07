Summary of changes from WRF to WRF-fire November 6, 2008

1. Subgrid support
------------------

The base WRF code is https://svn-wrf-model.cgd.ucar.edu/trunk as
of April 2, 2007 Last Changed Rev: 2285 Last Changed Date: 2007-04-02
10:10:25 -0600 (Mon, 02 Apr 2007). John Michalakes implemented
the subgrid support by changes to the files

external/RSL_LITE/gen_comms.c frame/module_domain.F tools/gen_wrf_io.c
tools/type.c tools/data.h tools/misc.c share/mediation_feedback_domain.F
Registry/registry.io_boilerplate dyn_em/solve_em.F 
and new file dyn_em/module_initialize_fire.F

The subgrid support enables declaration in the registry of refined
meshes on the finest WRF grid, essentially by allocating more memory
to them than what normally would be. Subgrid variables live on the same
domain as the finest WRF grid but they are 2D only and have more points
by refinement ratios sr_x sr_y, which are given in namelist.input 

The baseline code and John's modified code are stored in our git
repository in branch svnbranch', which has two commits that look
exactly like the file trees he sent us. The git repository can be
cloned from http://github.com/jbeezley/wrf-fire/tree/master or
http://repo.or.cz/w/wrffire.git

To get the changes that John made: 
 git diff wrfsvn wrfsubgrid 
or a summary all modifications from the bare subgrid version:
 git diff --stat wrfsubgrid origin/master


2. Coupling with the fire code
------------------------------

The following files were changed (started by Net Patton, continued
by the present developers):

 Registry/Registry.EM - new section to declare fire variables and halos,
   new variables sr_x sr_y for the refinement ratios in the domain section
 phys/module_physics_addtendc.F - code add the output of fire code 
   to heat and moisture tendencies
 dyn_em/solve_em.F - call to the fire driver 
 phys/Makefile - new entries for the fire model 
 Makefile (top level) - new target em_fire


3. The fire model code itself
-----------------------------

The fire model is in files phys/*_fr_sfire_* in the phys directory.
We'll support these files as well as the fire section in the registry,
the call to our our driver in dyn_em/solve_em.F, and our entries
in phys/Makefile

The fire model is written following WRF coding conventions. The
fire code itself is independent of the WRF implementation with the
following exceptions:

phys/module_fr_sfire_driver.F:
    USE module_domain USE module_configure USE module_driver_constants
    USE module_machine USE module_tiles USE module_dm
Used to extract state variables and grid definitions from the domain
structure, extract configuration flags obtained from the registry,
interpolate atmospheric variables to fire subgrid resolution, and
set up tile level (openmp) parallelism.  Any changes in WRF framework
could effect this file.  The driver in this file takes care of
openmp threading for the fire code, so it also contains several
halo exchange includes.  *Halo exchanging support is necessary in
the subgrid implementation.*

phys/module_fr_sfire_phys.F:
    use module_model_constants, only: cp,xlv
Encapsulates everything related to physics. All other files contain
mathematical algorithms only with no reference to the physics,
units, etc. Used to compute heat fluxes to the atmosphere.

phys/module_fr_sfire_util.F:
    use module_wrf_error - For printing debug and error messages
    use module_dm - For getting MPI process information for debug
    messages only.
Support routines such as interpolation and priting. All calls to
WRF routines pass through this module. It contains a number of
routines with I/O statements for debugging purposes. These can be
removed if necessary.

No other WRF standard modules or includes are used in the fire code.


4. Features not to be included in the WRF release
-------------------------------------------------

The following features are present in the master branch of our git
repository and will not be contained in the WRF release (which is
being prepared in the release branch of our repository):

Standalone model: The model is independent of WRF. Using phys/testmakefile
builds a completely standalone model. The standalone model replaces
module_fr_sfire_phys.F by a main program and uses the file
phys/wrf_fakes.F to simulate a subset of WRF infrastructure.

The CAWFE model: This is the original Clark-Coen model as ported
by Net Patton, implemented as files phys/*_fire_* and phys/*_cawfe_*,
and call fire_driver in dyn_em/solve_em.F


5. Fire data input and WPS
--------------------------

The fire code needs to have data inputs at the subgrid resolution.
At the moment, it is using fuel category data and interpolating
other data sources like elevation/slope and latitude/longitude from
WRF atmospheric resolution variables.  In the future, we may find
it necessary to import more subgrid resolution variables from
standard data sources.  

We are importing this data through a modified (to support subgrids)
version of WPS, and WRF imports it from wrfinput_d?? just like
every other data source.  The fuel category data we are currently
using comes from this website:

http://www.landfire.gov/viewer/

under 13 Anderson Fire Behavior Fuel Models.  The data from this
source needs to be converted into a geogrid compatible format, which
we currently don't have an automated method of accomplishing.  This
converted data is then placed with the rest of the geogrid data and
imported in the standard way.  The repository
contains the modified WPS with subgrid support that we are using.
It contains a definition of NFUEL_CAT for the GEOGRID.TBL for
importing the data. 


6. Impact on portability, performance, and parallelization
----------------------------------------------------------

The subgrid supports halo under RSL-LITE but not under RSL.  The
fire code can be used only with the EM core at the moment.

In our testing so far the fire code takes about 20% of CPU, less
for larger number of processes or threads. So, the fire component
scales better than WRF itself. The fire code is not optimized yet
so the % of time it takes may well further decrease.

The fire code has been written to conform to WRF physics coding
standards as possible.  There are no direct calls to MPI routines,
I/O, memory allocations, etc.  It is written to Fortran 90 standards.
We primarily do development work on ifort v10.1 on Linux; however,
we have tested the code on the following compilers/platforms without
issues:

ifort v10.1/v11/gcc Linux; pgf90 v6.x/pgcc Linux (NCAR's walnut.mmm);
g95/gcc MacOS 10.5; xlf90/xlc on NCAR's frost

The current support of subgrid data in WPS should be considered
preliminary and it runs on a single processor and thread only.

The fire code can be deactivated at run-time through the namelist
variable ifire, in which case, the entire fire code will be skipped.
There is little performance penalty from a non-fire build.  Any
difference is a result of increased allocated (but unused) memory
from the fire state variables and additional file I/O into restart
and history files.

