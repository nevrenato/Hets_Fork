#ifndef __BREP_TO_XML_HXX
#define __BREP_TO_XML_HXX
#include <BRepTools.hxx>
#include <TopoDS.hxx>
#include <TopoDS_Shape.hxx>
#include <TopAbs_ShapeEnum.hxx>
#include <BRep_Builder.hxx>
#include <BRepTools_ShapeSet.hxx>
#include <Standard_TypeDef.hxx>
#include <Standard_Stream.hxx>
#include <TopoDS_Compound.hxx>
#include <TopoDS_CompSolid.hxx>
#include <TopoDS_Edge.hxx>
#include <TopoDS_Face.hxx>
#include <TopoDS_Shell.hxx>
#include <TopoDS_Solid.hxx>
#include <TopoDS_Vertex.hxx>
#include <TopoDS_Wire.hxx>
#include <TopoDS_Iterator.hxx>
#include <vector>
#include <utility>
#include <gp_Pnt.hxx>
#include <libxml/encoding.h>
#include <libxml/xmlwriter.h>

class BrepToXML
{
private:
    TopoDS_Shape Sh;
    BRep_Builder builder;
    BRepTools_ShapeSet SS;
    
    std::vector < std::vector < int > > graph;
    std::vector < std::pair < int, gp_Pnt > > vLocs;
    
    void add_to_graph(void);
    void init_graph(void);
    void simplify_graph(void);
    void cacheProperties(void);
    void cacheProperties(const TopoDS_Shape&); 
public:

    BrepToXML();
    BrepToXML(TopoDS_Shape);
    BrepToXML(const BrepToXML&);
    ~BrepToXML();
    
    TopoDS_Shape get_shape(void);
    void set_shape(TopoDS_Shape);
    
    bool read_brep(const char* filePath); //returns 1 if reading error occurs, 0 otherwise
    
    void print_shape_type(TopoDS_Shape);
    
    void print_subshapes(void);
    
    std::vector <TopoDS_Shape> get_subshapes(int);
    void build_graph(void);
    void print_graph(void);
    void build_xml(const std::string&, std::string&);
};
#endif
