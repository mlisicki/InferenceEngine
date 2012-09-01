Basic inference engine
=========================================

Description
-----------

Basic inference engine for 3D object recognition with chair model (created in Blender) as use case.

Knowledge base structure is based on ideas covered in "Integration of different computational models in a computer vision framework" by Wlodziemirz Kasprzak.

The application can be run using the command:

run.sh [file.obj]

where [file.obj] is a wavefront file (e.g. chair.obj in provided example). The output is the full description of the
inferred object. As it is recursive and includes the descriptions of all of its components I recommend using the Prolog environment for more comprehensive review (use the "[load]." command within the interpreter run in the project director). Although all the example procedures can be easily read looking at the source code, I provide a few more interresting exmples:

- object(vertex,Grpah,Position).

- object(segment,Graph,Position).

- octree(segment,Octree,NewVertices).

- show(Octree).

- O1 =.. [object,segment,G1,P1], O2 =.. [object,segment,G2,P2], call(O1), call(O2), cv_arc(O1,O2,connected,_).

- object(chair,Graph,Position).

Dependencies
------------

- SWI-Prolog


Maintainer
----------

Michal Lisicki

