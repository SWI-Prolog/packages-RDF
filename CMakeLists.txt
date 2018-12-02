cmake_minimum_required(VERSION 2.8.12)
project(swipl-RDF)

include("../cmake/PrologPackage.cmake")

has_package(xpce HAVE_XPCE)

swipl_plugin(RDF
	     PL_LIBS rdf.pl rdf_parser.pl rdf_triple.pl rewrite.pl rdf_write.pl)
if(HAVE_XPCE)
  swipl_plugin(XPCERDF PL_LIBS rdf_diagram.pl)
endif()

test_libs(
    rdf write
    PACKAGES clib sgml semweb
    TEST_FILES w3c_ntdata.pl)

pkg_doc(
    rdf2pl
    DEPENDS RDF)
