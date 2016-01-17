unit Glaux;

interface

uses Windows,Opengl;

type
	TAUX_RGBImageRec= record
		sizeX, sizeY: Integer;
		data: pointer;
	end;
	PTAUX_RGBImageRec= ^TAUX_RGBImageRec;

function auxDIBImageLoadA(const dibfile: PChar): PTAUX_RGBImageRec; stdcall;
procedure auxWireSphere(value: Double);stdcall;
procedure auxSolidSphere(value: Double);stdcall;
procedure auxWireCube(value: Double);stdcall;
procedure auxSolidCube(value: Double);stdcall;
procedure auxWireBox(value,value1,value2: Double);stdcall;
procedure auxSolidBox(value,value1,value2: Double);stdcall;
procedure auxWireTorus(value,value1: Double);stdcall;
procedure auxSolidTorus(value,value1: Double);stdcall;
procedure auxWireCylinder(value,value1: Double);stdcall;
procedure auxSolidCylinder(value,value1: Double);stdcall;
procedure auxWireIcosahedron(value: Double);stdcall;
procedure auxSolidIcosahedron(value: Double);stdcall;
procedure auxWireOctahedron(value: Double);stdcall;
procedure auxSolidOctahedron(value: Double);stdcall;
procedure auxWireTetrahedron(value: Double);stdcall;
procedure auxSolidTetrahedron(value: Double);stdcall;
procedure auxWireDodecahedron(value: Double);stdcall;
procedure auxSolidDodecahedron(value: Double);stdcall;
procedure auxWireCone(value,value1: Double);stdcall;
procedure auxSolidCone(value,value1: Double);stdcall;
procedure auxWireTeapot(value: Double);stdcall;
procedure auxSolidTeapot(value: Double);stdcall;

const
	glaux1 = 'glaux.dll';
           
implementation

function auxDIBImageLoadA; external glaux1;
procedure auxWireSphere;external glaux1;
procedure auxSolidSphere;external glaux1;
procedure auxWireCube;external glaux1;
procedure auxSolidCube;external glaux1;
procedure auxWireBox;external glaux1;
procedure auxSolidBox;external glaux1;
procedure auxWireTorus;external glaux1;
procedure auxSolidTorus;external glaux1;
procedure auxWireCylinder;external glaux1;
procedure auxSolidCylinder;external glaux1;
procedure auxWireIcosahedron;external glaux1;
procedure auxSolidIcosahedron;external glaux1;
procedure auxWireOctahedron;external glaux1;
procedure auxSolidOctahedron;external glaux1;
procedure auxWireTetrahedron;external glaux1;
procedure auxSolidTetrahedron;external glaux1;
procedure auxWireDodecahedron;external glaux1;
procedure auxSolidDodecahedron;external glaux1;
procedure auxWireCone;external glaux1;
procedure auxSolidCone;external glaux1;
procedure auxWireTeapot;external glaux1;
procedure auxSolidTeapot;external glaux1;


end.
