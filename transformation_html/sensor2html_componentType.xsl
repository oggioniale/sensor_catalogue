<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:xs="http://www.w3.org/2001/XMLSchema"
    xmlns:swes="http://www.opengis.net/swes/2.0"
    xmlns:sos="http://www.opengis.net/sos/2.0"
    xmlns:swe="http://www.opengis.net/swe/2.0"
    xmlns:sml="http://www.opengis.net/sensorml/2.0"
    xmlns:gml="http://www.opengis.net/gml/3.2"
    xmlns:sams="http://www.opengis.net/samplingSpatial/2.0"
    xmlns:sf="http://www.opengis.net/sampling/2.0"
    xmlns:gco="http://www.isotc211.org/2005/gco"
    xmlns:gmd="http://www.isotc211.org/2005/gmd"
    xmlns:xlink="http://www.w3.org/1999/xlink"
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    xmlns:skos="http://www.w3.org/2004/02/skos/core#"
    xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
    xmlns:getit="http://rdfdata.get-it.it/"
    xmlns:foaf="http://xmlns.com/foaf/0.1/"
    xmlns:j.0="http://xxx.com"
    exclude-result-prefixes="xs" version="2.0">
    
    <xsl:output method="html" doctype-system="about:legacy-compat" encoding="UTF-8" indent="yes"/>
    
    <xsl:strip-space elements="*" />
    
    <xsl:template match="/">
        <html lang="en">
            <head>
                <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
                
                <meta http-equiv="X-UA-Compatible" content="IE=edge"/>
                <meta name="viewport" content="width=device-width, initial-scale=1"/>
                <meta name="description"
                    content="Human readable version of a sensor description from SensorML"/>
                <meta name="author" content="Alessandro Oggioni"/>
                <link rel="icon" href="https://www.lteritalia.it/wordpress/wp-content/uploads/2023/09/solo_foglia.png"/>
                <title>Sensor system type description</title>
                
                <link href="//www.get-it.it/objects/types/transformation_html/assets/css/font-awesome.min.css" rel="stylesheet"/>
                
                <style type="text/css">
                    .tldate {
                    border: 1px solid #d4d4d4;
                    border-radius: 2px;
                    -webkit-box-shadow: 0 1px 6px rgba(0, 0, 0, 0.175);
                    box-shadow: 0 1px 6px rgba(0, 0, 0, 0.175);
                    display: block;
                    width: 200px;
                    background: #999999;
                    /*background: #414141;*/
                    /*border: 3px solid #212121;*/
                    color: #ededed;
                    margin: 0 auto;
                    padding: 3px 0;
                    font-weight: bold;
                    text-align: center;
                    /*-webkit-box-shadow: 0 0 11px rgba(0,0,0,0.35);*/
                    }
                    
                    .span4 {
                    height: 100%;
                    overflow: auto;
                    }
                    
                    #map {
                    position: absolute;
                    width: 100%;
                    height: 400px;
                    margin: 0;
                    padding: 0;
                    border: 1px solid #E5E5E5;
                    border-radius: 8px;
                    }
                    
                    #mapRow {
                    height: 300px;
                    }
                    
                    #map-outer {
                    height: 440px;
                    padding: 20px;
                    border: 2px solid #CCC;
                    margin-bottom: 20px;
                    background-color: #FFF
                    }
                    
                    #map-container {
                    height: 400px
                    }
                    
                    @media all and (max-width : 768px) {
                    #map-outer {
                    height: 650px
                    }
                    }
                    
                    
                    /*  bhoechie tab */
                    div.bhoechie-tab-container {
                    z-index: 10;
                    background-color: #ffffff;
                    padding: 0 !important;
                    border-radius: 4px;
                    -moz-border-radius: 4px;
                    border: 1px solid #ddd;
                    margin-top: 20px;
                    margin-left: 50px;
                    -webkit-box-shadow: 0 6px 12px rgba(0, 0, 0, .175);
                    box-shadow: 0 6px 12px rgba(0, 0, 0, .175);
                    -moz-box-shadow: 0 6px 12px rgba(0, 0, 0, .175);
                    background-clip: padding-box;
                    opacity: 0.97;
                    filter: alpha(opacity=97);
                    }
                    div.bhoechie-tab-menu {
                    padding-right: 0;
                    padding-left: 0;
                    padding-bottom: 0;
                    }
                    div.bhoechie-tab-menu div.list-group {
                    margin-bottom: 0;
                    }
                    div.bhoechie-tab-menu div.list-group > a {
                    margin-bottom: 0;
                    }
                    div.bhoechie-tab-menu div.list-group > a .glyphicon,
                    div.bhoechie-tab-menu div.list-group > a .fa {
                    color: #5A55A3;
                    }
                    div.bhoechie-tab-menu div.list-group > a:first-child {
                    border-top-right-radius: 0;
                    -moz-border-top-right-radius: 0;
                    }
                    div.bhoechie-tab-menu div.list-group > a:last-child {
                    border-bottom-right-radius: 0;
                    -moz-border-bottom-right-radius: 0;
                    }
                    div.bhoechie-tab-menu div.list-group > a.active,
                    div.bhoechie-tab-menu div.list-group > a.active .glyphicon,
                    div.bhoechie-tab-menu div.list-group > a.active .fa {
                    background-color: #5A55A3;
                    background-image: #5A55A3;
                    color: #ffffff;
                    }
                    div.bhoechie-tab-menu div.list-group > a.active:after {
                    content: '';
                    position: absolute;
                    left: 100%;
                    top: 50%;
                    margin-top: -13px;
                    border-left: 0;
                    border-bottom: 13px solid transparent;
                    border-top: 13px solid transparent;
                    border-left: 10px solid #5A55A3;
                    }
                    
                    div.bhoechie-tab-content {
                    background-color: #ffffff;
                    /* border: 1px solid #eeeeee; */
                    padding-left: 20px;
                    padding-top: 10px;
                    }
                    
                    div.bhoechie-tab div.bhoechie-tab-content:not(.active) {
                    display: none;
                    }
                </style>
                
                <!--<script src="https://unpkg.com/leaflet@1.9.4/dist/leaflet.js" integrity="sha256-20nQCchB9co0qIjJZRGuk2/Z9VM+kNiyxNV1lvTlZBo=" crossorigin=""/>-->
                
                <!-- DataTables -->
                <link rel="stylesheet"
                    href="//www.get-it.it/objects/types/transformation_html/assets/css/bootstrap-3.3.7.min.css"/>
                <link rel="stylesheet" href="//www.get-it.it/objects/types/transformation_html/assets/DataTables/datatables.css"/>
                
                <script type="text/javascript" src="//www.get-it.it/objects/types/transformation_html/assets/js/jquery-1.12.4.js"/>
                <script type="text/javascript" src="//www.get-it.it/objects/types/transformation_html/assets/js/jquery-1.10.16.dataTables.min.js"/>
                <script type="text/javascript" src="//www.get-it.it/objects/types/transformation_html/assets/DataTables/datatables.js"/>
                
                <script type="text/javascript">
                    $(document).ready(function () {
                    $('#example').DataTable();
                    });</script>
                
                <style>
                    .quote-card {
                    background: #fff;
                    color: #222222;
                    padding: 20px;
                    padding-left: 50px;
                    box-sizing: border-box;
                    box-shadow: 0 2px 4px rgba(34, 34, 34, 0.12);
                    position: relative;
                    overflow: hidden;
                    min-height: 120px;
                    }
                    .quote-card p {
                    font-size: 22px;
                    line-height: 1.5;
                    margin: 0;
                    max-width: 80%;
                    }
                    .quote-card cite {
                    font-size: 16px;
                    margin-top: 10px;
                    display: block;
                    font-weight: 200;
                    opacity: 0.8;
                    }
                    .quote-card:before {
                    font-family: Georgia, serif;
                    content: "“";
                    position: absolute;
                    top: 10px;
                    left: 10px;
                    font-size: 5em;
                    color: rgba(238, 238, 238, 0.8);
                    font-weight: normal;
                    }
                    .quote-card:after {
                    font-family: Georgia, serif;
                    content: "”";
                    position: absolute;
                    bottom: -110px;
                    line-height: 100px;
                    right: -32px;
                    font-size: 5em;
                    color: rgba(238, 238, 238, 0.8);
                    font-weight: normal;
                    }
                    @media (max-width : 640px) {
                    .quote-card:after {
                    font-size: 22em;
                    right: -25px;
                    }
                    }</style>
                
                <script src="//www.get-it.it/objects/types/transformation_html/assets/js/bootstrap-3.0.3.min.js" type="text/javascript"/>
                
                <script src="//www.get-it.it/objects/types/transformation_html/assets/js/SaxonJS2.js"/>
                
            </head>
            
            <body>
                
                <div class="container">
                    <!-- Navbar -->
                    <nav class="navbar navbar-light">
                        <!-- main navigation bar -->
                        <div class="container-fluid" style="background-color: #334155;margin-top: 19px;">
                            <div class="navbar-header">
                                <a class="navbar-brand" href="http://www.lteritalia.it" target="_blank">
                                    <img src="https://www.lteritalia.it/wordpress/wp-content/uploads/LTER-IT-033-300x89.png"
                                        height="50"
                                        style="padding-top: 0px;margin-top: -19px;"/>
                                </a>
                            </div>
                            <ul class="nav navbar-nav navbar-right">
                                <span style="color: #94c5e5"><strong><li class="navbar-text">Sensor component landing page</li></strong></span>
                            </ul>
                        </div>
                        <!-- secondary navigation bar -->
                        
                    </nav>
                    <!-- End Navbar -->
                    
                    <!-- Central -->
                    <div class="row row-offcanvas row-offcanvas-right">
                        <div class="bd-cheatsheet container-fluid bg-body">
                            <div class="row flex">
                                <!-- side menu -->
                                <div class="col-xs-2 col-xs-offset-1 col-sm-2 col-sm-offset-0">
                                    <div class="container">
                                        <div class="row">
                                            <div class="col-xs-2">
                                                <aside class="bd-aside sticky-xl-top text-muted align-self-start mb-3 mb-xl-5 px-2">
                                                    <h2 class="h6 pt-4 pb-3 mb-4 border-bottom">Sensor component information</h2>
                                                    <nav class="small" id="toc">
                                                        <ul class="list-unstyled">
                                                            <li class="my-2">
                                                                <ul class="list-unstyled ps-3 collapse show" id="contents-collapse" style="">
                                                                    <li><a class="d-inline-flex align-items-center rounded" href="#decription">Description</a></li>
                                                                    <li><a class="d-inline-flex align-items-center rounded" href="#classification">Classification</a></li>
                                                                    <li><a class="d-inline-flex align-items-center rounded" href="#capabilities">Capabilities</a></li>
                                                                    <li><a class="d-inline-flex align-items-center rounded" href="#components">Attacted to</a></li>
                                                                </ul>
                                                            </li>
                                                        </ul>
                                                    </nav>
                                                </aside>
                                            </div>
                                        </div>
                                    </div>
                                </div>
                                <!-- main body -->
                                <div class="col-xs-18 col-xs-offset-1 col-sm-10 col-sm-offset-0">
                                    <div class="container">
                                        <div class="row">
                                            <div class="col-xs-9">
                                                <section id="content">
                                                    <!--<h2 class="sticky-xl-top fw-bold pt-3 pt-xl-5 pb-2 pb-xl-3">Contents</h2>-->
                                                    <p class="h1">
                                                        <xsl:value-of select="//sml:PhysicalComponent/gml:name"/>
                                                        <xsl:text> </xsl:text>
                                                        <xsl:if
                                                            test="//sml:identification/sml:IdentifierList/sml:identifier/sml:Term[@definition='http://vocab.nerc.ac.uk/collection/W07/current/IDEN0006/']/sml:value/text()">
                                                        </xsl:if>
                                                    </p>
                                                    <p class="h4">
                                                        <xsl:if test="//sml:PhysicalComponent/gml:identifier[@codeSpace='uniqueID']/text()">
                                                            <a href="{//sml:PhysicalComponent/gml:identifier[@codeSpace='uniqueID']/text()}" target="_blank">
                                                                <xsl:value-of select="//sml:PhysicalComponent/gml:identifier[@codeSpace='uniqueID']/text()" />
                                                            </a>
                                                        </xsl:if>
                                                    </p>
                                                    <h4>
                                                        <small>
                                                            <b>Download: </b>
                                                            <xsl:variable name="contactURI" select="//sml:contacts/sml:ContactList/sml:contact/@xlink:href"/> <!-- e.g. http://rdfdata.get-it.it/sensors/manufacturers/83 -->
                                                            <xsl:variable name="contactName" select="//sml:contacts/sml:ContactList/sml:contact/@xlink:title"/> <!-- e.g. Nortek -->
                                                            <xsl:variable name="contactXMLLodViewPage" select="concat('http://lodview.get-it.it/lodview/?sparql=http://fuseki1.get-it.it/manufacturers/query&amp;IRI=', $contactURI, '&amp;output=application/rdf+xml')"/> <!-- http://lodview.get-it.it/lodview/?sparql=http://fuseki1.get-it.it/manufacturers/query&IRI=http://rdfdata.get-it.it/sensors/manufacturers/83&output=application/rdf+xml -->
                                                            <xsl:variable name="contactJSONLodViewPage" select="concat('http://lodview.get-it.it/lodview/?sparql=http://fuseki1.get-it.it/manufacturers/query&amp;IRI=', $contactURI, '&amp;output=application/ld+json')"/> <!-- http://lodview.get-it.it/lodview/?sparql=http://fuseki1.get-it.it/manufacturers/query&IRI=http://rdfdata.get-it.it/sensors/manufacturers/83&output=application/ld+json -->
                                                            <b>
                                                                <a role="button" class="btn btn-success btn-xs"
                                                                    href="{$contactXMLLodViewPage}" download="{$contactXMLLodViewPage}" target="_blank">
                                                                    <svg xmlns="http://www.w3.org/2000/svg" width="16"
                                                                        height="16" fill="currentColor" class="bi bi-download"
                                                                        viewBox="0 0 16 16">
                                                                        <path
                                                                            d="M.5 9.9a.5.5 0 0 1 .5.5v2.5a1 1 0 0 0 1 1h12a1 1 0 0 0 1-1v-2.5a.5.5 0 0 1 1 0v2.5a2 2 0 0 1-2 2H2a2 2 0 0 1-2-2v-2.5a.5.5 0 0 1 .5-.5"/>
                                                                        <path
                                                                            d="M7.646 11.854a.5.5 0 0 0 .708 0l3-3a.5.5 0 0 0-.708-.708L8.5 10.293V1.5a.5.5 0 0 0-1 0v8.793L5.354 8.146a.5.5 0 1 0-.708.708z"
                                                                        />
                                                                    </svg> SensorML-XML </a>
                                                            </b>
                                                            <xsl:text> </xsl:text>
                                                            <b>
                                                                <a role="button" class="btn btn-success btn-xs"
                                                                    href="{$contactXMLLodViewPage}" download="{$contactXMLLodViewPage}" target="_blank">
                                                                    <svg xmlns="http://www.w3.org/2000/svg" width="16"
                                                                        height="16" fill="currentColor" class="bi bi-download"
                                                                        viewBox="0 0 16 16">
                                                                        <path
                                                                            d="M.5 9.9a.5.5 0 0 1 .5.5v2.5a1 1 0 0 0 1 1h12a1 1 0 0 0 1-1v-2.5a.5.5 0 0 1 1 0v2.5a2 2 0 0 1-2 2H2a2 2 0 0 1-2-2v-2.5a.5.5 0 0 1 .5-.5"/>
                                                                        <path
                                                                            d="M7.646 11.854a.5.5 0 0 0 .708 0l3-3a.5.5 0 0 0-.708-.708L8.5 10.293V1.5a.5.5 0 0 0-1 0v8.793L5.354 8.146a.5.5 0 1 0-.708.708z"
                                                                        />
                                                                    </svg> Turtle RDF-XML </a>
                                                            </b>
                                                            <xsl:text> </xsl:text>
                                                            <b>
                                                                <a role="button" class="btn btn-success btn-xs"
                                                                    href="{$contactXMLLodViewPage}" download="{$contactXMLLodViewPage}" target="_blank">
                                                                    <svg xmlns="http://www.w3.org/2000/svg" width="16"
                                                                        height="16" fill="currentColor" class="bi bi-download"
                                                                        viewBox="0 0 16 16">
                                                                        <path
                                                                            d="M.5 9.9a.5.5 0 0 1 .5.5v2.5a1 1 0 0 0 1 1h12a1 1 0 0 0 1-1v-2.5a.5.5 0 0 1 1 0v2.5a2 2 0 0 1-2 2H2a2 2 0 0 1-2-2v-2.5a.5.5 0 0 1 .5-.5"/>
                                                                        <path
                                                                            d="M7.646 11.854a.5.5 0 0 0 .708 0l3-3a.5.5 0 0 0-.708-.708L8.5 10.293V1.5a.5.5 0 0 0-1 0v8.793L5.354 8.146a.5.5 0 1 0-.708.708z"
                                                                        />
                                                                    </svg> JSON-LD </a>
                                                            </b>
                                                        </small>
                                                    </h4>
                                                    <xsl:if test="//sml:keywords/sml:KeywordList/sml:keyword">
                                                        <h4><small>
                                                            <b>Keywords: </b>
                                                            <xsl:for-each select="//sml:PhysicalComponent/sml:keywords/sml:KeywordList/sml:keyword">
                                                                <xsl:if test="not(contains(., 'http://')) and not(contains(., 'offering:')) and not(contains(., 'https://'))">
                                                                        <xsl:value-of select="."/><xsl:text>,  </xsl:text>
                                                                </xsl:if>
                                                            </xsl:for-each>
                                                        </small></h4>
                                                    </xsl:if>
                                                    <hr/>
                                                    <article class="my-3" id="decription">
                                                        <div class="bd-heading sticky-xl-top align-self-start mt-5 mb-3 mt-xl-0 mb-xl-2">
                                                            <h4>Description</h4>
                                                            <xsl:call-template name="description"/>
                                                        </div>
                                                    </article>
                                                    <hr/>
                                                    <article class="my-3" id="classification">
                                                        <div class="bd-heading sticky-xl-top align-self-start mt-5 mb-3 mt-xl-0 mb-xl-2">
                                                            <h4>Classification</h4>
                                                            <xsl:call-template name="classification"/>
                                                        </div>
                                                    </article>
                                                    <hr/>
                                                    <article class="my-3" id="capabilities">
                                                        <div class="bd-heading sticky-xl-top align-self-start mt-5 mb-3 mt-xl-0 mb-xl-2">
                                                            <h4>Capabilities</h4>
                                                            <xsl:call-template name="capabilities"/>
                                                        </div>
                                                    </article>
                                                    <hr/>
                                                    <article class="my-3" id="attactedTo">
                                                        <div class="bd-heading sticky-xl-top align-self-start mt-5 mb-3 mt-xl-0 mb-xl-2">
                                                            <h4>Attached to</h4>
                                                            <xsl:call-template name="attactedTo"/>
                                                        </div>
                                                    </article>
                                                </section>
                                            </div>
                                        </div>
                                    </div>
                                </div>
                                
                            </div>
                        </div>
                        
                    </div>
                    <!--/row-->
                    <!-- End Central -->
                    
                    <!-- Site footer -->
                    <footer class="footer">
                        <hr/>
                        <div class="col-lg-6">
                            <p>
                                <span style="color: #94c5e5"><strong>Contacts</strong></span><br/>
                                <strong>Secretariat: </strong>Via Roberto Cozzi, 53 20156 Milan (Italy)<br/>
                                <strong>Phone: </strong>+02 66173307<br/>
                                <strong>E-mail: </strong><a href="mailto:lteritaly@gmail.com" target="_blank">lteritaly@gmail.com</a>
                            </p>
                        </div>
                        <div class="col-lg-6">
                            <span style="color: #94c5e5"><strong>Useful links</strong></span><br/>
                            <a href="http://sparql.lteritalia.it/" target="_blank">SPARQL Endpoint</a>
                        </div>
                    </footer>
                    <!-- End Site footer -->
                    
                </div>
                <!--/.container-->
            </body>
        </html>
    </xsl:template>
    
    <!-- template description -->
    <xsl:template name="description">
        <div>
            <div class="row row-cols-1 row-cols-md-2 g-4">
                <div class="col">
                    <xsl:if test="//gml:description">
                        <p>
                            <xsl:value-of select="//gml:description"/>
                        </p>
                    </xsl:if>
                </div>
            </div>
        </div>
    </xsl:template>
    
    <!-- template classification -->
    <xsl:template name="classification">
        <div>
            <xsl:if test="//sml:classification/sml:ClassifierList/sml:classifier/sml:Term[@definition='http://www.opengis.net/def/property/OGC/0/SensorType']">
                <ul class="list-group">
                    <xsl:for-each select="//sml:classification/sml:ClassifierList/sml:classifier">
                        <li class="list-group-item">
                            <a href='{./sml:Term/sml:value/text()}' target='_blank'><xsl:value-of select="./sml:Term/sml:label/text()"/></a>
                        </li>
                    </xsl:for-each>
                </ul>
            </xsl:if>
        </div>
    </xsl:template>
    
    <!-- template capabilities -->
    <xsl:template name="capabilities">
        <div>
            <!-- electricalRequirements -->
            <xsl:if test="//sml:capabilities[@name='capabilities']/sml:CapabilityList">
                <table class="table table-striped">
                    <thead>
                        <tr>
                            <th scope="col">Capability</th>
                            <th scope="col">Value(s)</th>
                            <th scope="col">unit of measure</th>
                        </tr>
                    </thead>
                    <tbody>
                        <!-- offeringID -->
                        <!--xsl:if test="//sml:capability[@name='offeringID']">
                                                xxx
                                            </xsl:if-->
                        <!-- Accuracy -->
                        <xsl:if test="//sml:capability[@name='Accuracy']/swe:Quantity">
                            <xsl:variable name="accuracyURI" select="//sml:capability[@name='Accuracy']/swe:Quantity/@definition"/>
                            <xsl:variable name="accuracy_uomURI" select="//sml:capability[@name='Accuracy']/swe:Quantity/swe:uom/@xlink:href"/>
                            <tr>
                                <td><a href="{$accuracyURI}" target="_blank">Accuracy</a></td>
                                <td><xsl:value-of select="//sml:capability[@name='Accuracy']/swe:Quantity/swe:value"/></td>
                                <td><a href='{$accuracy_uomURI}' target="_blank"><xsl:value-of select="//sml:capability[@name='Accuracy']/swe:Quantity/swe:uom/@code"/></a></td>
                            </tr>
                        </xsl:if>
                        <xsl:if test="//sml:capability[@name='Accuracy']/swe:QuantityRange">
                            <xsl:variable name="accuracyURI" select="//sml:capability[@name='Accuracy']/swe:QuantityRange/@definition"/>
                            <xsl:variable name="accuracy_uomURI" select="//sml:capability[@name='Accuracy']/swe:QuantityRange/swe:uom/@xlink:href"/>
                            <tr>
                                <td><a href="{$accuracyURI}" target="_blank">Accuracy</a></td>
                                <td><xsl:value-of select="//sml:capability[@name='Accuracy']/swe:QuantityRange/swe:value"/></td>
                                <td><a href='{$accuracy_uomURI}' target="_blank"><xsl:value-of select="//sml:capability[@name='Accuracy']/swe:QuantityRange/swe:uom/@code"/></a></td>
                            </tr>
                        </xsl:if>
                        <xsl:if test="//sml:capability[@name='Accuracy']/swe:Text">
                            <xsl:variable name="accuracyURI" select="//sml:capability[@name='Accuracy']/swe:Text/@definition"/>
                            <tr>
                                <td><a href="{$accuracyURI}" target="_blank">Accuracy</a></td>
                                <td><xsl:value-of select="//sml:capability[@name='Accuracy']/swe:Text/swe:value"/></td>
                                <td>-</td>
                            </tr>
                        </xsl:if>
                        <!-- MeasurementRange -->
                        <xsl:if test="//sml:capability[@name='MeasurementRange']/swe:QuantityRange">
                            <xsl:variable name="measurementRangeURI" select="//sml:capability[@name='MeasurementRange']/swe:QuantityRange/@definition"/>
                            <xsl:variable name="measurementRange_uomURI" select="//sml:capability[@name='MeasurementRange']/swe:QuantityRange/swe:uom/@xlink:href"/>
                            <tr>
                                <td><a href="{$measurementRangeURI}" target="_blank">Measurement Range</a></td>
                                <td><xsl:value-of select="//sml:capability[@name='MeasurementRange']/swe:QuantityRange/swe:value"/></td>
                                <td><a href='{$measurementRange_uomURI}' target="_blank"><xsl:value-of select="//sml:capability[@name='MeasurementRange']/swe:QuantityRange/swe:uom/@code"/></a></td>
                            </tr>
                        </xsl:if>
                        <!-- OperatingDepth -->
                        <xsl:if test="//sml:capability[@name='OperatingDepth']/swe:Quantity">
                            <xsl:variable name="operatingDepthURI" select="//sml:capability[@name='OperatingDepth']/swe:Quantity/@definition"/>
                            <xsl:variable name="operatingDepth_uomURI" select="//sml:capability[@name='OperatingDepth']/swe:Quantity/swe:uom/@xlink:href"/>
                            <tr>
                                <td><a href="{$operatingDepthURI}" target="_blank">Operating Depth</a></td>
                                <td><xsl:value-of select="//sml:capability[@name='OperatingDepth']/swe:Quantity/swe:value"/></td>
                                <td><a href='{$operatingDepth_uomURI}' target="_blank"><xsl:value-of select="//sml:capability[@name='OperatingDepth']/swe:Quantity/swe:uom/@code"/></a></td>
                            </tr>
                        </xsl:if>
                        <!-- Precision -->
                        <xsl:if test="//sml:capability[@name='Precision']/swe:Quantity">
                            <xsl:variable name="precisionURI" select="//sml:capability[@name='Precision']/swe:Quantity/@definition"/>
                            <xsl:variable name="precision_uomURI" select="//sml:capability[@name='Precision']/swe:Quantity/swe:uom/@xlink:href"/>
                            <tr>
                                <td><a href="{$precisionURI}" target="_blank">Precision</a></td>
                                <td><xsl:value-of select="//sml:capability[@name='Precision']/swe:Quantity/swe:value"/></td>
                                <td><a href='{$precision_uomURI}' target="_blank"><xsl:value-of select="//sml:capability[@name='Precision']/swe:Quantity/swe:uom/@code"/></a></td>
                            </tr>
                        </xsl:if>
                        <!-- Resolution -->
                        <xsl:if test="//sml:capability[@name='Resolution']/swe:Quantity">
                            <xsl:variable name="resolutionURI" select="//sml:capability[@name='Resolution']/swe:Quantity/@definition"/>
                            <xsl:variable name="resolution_uomURI" select="//sml:capability[@name='Resolution']/swe:Quantity/swe:uom/@xlink:href"/>
                            <tr>
                                <td><a href="{$resolutionURI}" target="_blank">Resolution</a></td>
                                <td><xsl:value-of select="//sml:capability[@name='Resolution']/swe:Quantity/swe:value"/></td>
                                <td><a href='{$resolution_uomURI}' target="_blank"><xsl:value-of select="//sml:capability[@name='Resolution']/swe:Quantity/swe:uom/@code"/></a></td>
                            </tr>
                        </xsl:if>
                        <xsl:if test="//sml:capability[@name='Resolution']/swe:QuantityRange">
                            <xsl:variable name="resolutionURI" select="//sml:capability[@name='Resolution']/swe:QuantityRange/@definition"/>
                            <xsl:variable name="resolution_uomURI" select="//sml:capability[@name='Resolution']/swe:QuantityRange/swe:uom/@xlink:href"/>
                            <tr>
                                <td><a href="{$resolutionURI}" target="_blank">Resolution</a></td>
                                <td><xsl:value-of select="//sml:capability[@name='Resolution']/swe:QuantityRange/swe:value"/></td>
                                <td><a href='{$resolution_uomURI}' target="_blank"><xsl:value-of select="//sml:capability[@name='Resolution']/swe:QuantityRange/swe:uom/@code"/></a></td>
                            </tr>
                        </xsl:if>
                        <!-- Sensitivity -->
                        <xsl:if test="//sml:capability[@name='Sensitivity']/swe:Quantity">
                            <xsl:variable name="sensitivityURI" select="//sml:capability[@name='Sensitivity']/swe:Quantity/@definition"/>
                            <xsl:variable name="sensitivity_uomURI" select="//sml:capability[@name='Sensitivity']/swe:Quantity/swe:uom/@xlink:href"/>
                            <tr>
                                <td><a href="{$sensitivityURI}" target="_blank">Sensitivity</a></td>
                                <td><xsl:value-of select="//sml:capability[@name='Sensitivity']/swe:Quantity/swe:value"/></td>
                                <td><a href='{$sensitivity_uomURI}' target="_blank"><xsl:value-of select="//sml:capability[@name='Sensitivity']/swe:Quantity/swe:uom/@code"/></a></td>
                            </tr>
                        </xsl:if>
                        <!-- MinimumReportingFrequency -->
                        <xsl:if test="//sml:capability[@name='MinimumReportingFrequency']/swe:Quantity">
                            <xsl:variable name="minimumReportingFrequencyURI" select="//sml:capability[@name='MinimumReportingFrequency']/swe:Quantity/@definition"/>
                            <xsl:variable name="minimumReportingFrequency_uomURI" select="//sml:capability[@name='MinimumReportingFrequency']/swe:Quantity/swe:uom/@xlink:href"/>
                            <tr>
                                <td><a href="{$minimumReportingFrequencyURI}" target="_blank">Minimum Reporting Frequency</a></td>
                                <td><xsl:value-of select="//sml:capability[@name='MinimumReportingFrequency']/swe:Quantity/swe:value"/></td>
                                <td><a href='{$minimumReportingFrequency_uomURI}' target="_blank"><xsl:value-of select="//sml:capability[@name='MinimumReportingFrequency']/swe:Quantity/swe:uom/@code"/></a></td>
                            </tr>
                        </xsl:if>
                    </tbody>
                </table>
            </xsl:if>
        </div>
    </xsl:template>

    <!-- template attactedTo -->
    <xsl:template name="attactedTo">
        <div>
            <xsl:if test="//sml:attachedTo">
                <div class="list-group">
                    <xsl:variable name="uuidSystem" select="substring(//sml:PhysicalComponent/sml:attachedTo/@xlink:title, string-length(//sml:PhysicalComponent/sml:attachedTo/@xlink:title) - 35)"/>
                    <xsl:variable name="systemPath" select="concat('../sensorML_files_system_', $uuidSystem, '/system/ID_system_', $uuidSystem, '.xml')"/>
                    <xsl:variable name="systemURL" select="concat('../system/ID_system_', $uuidSystem, '.xml')"/>
                    <a class="list-group-item list-group-item-action" href='{$systemURL}' target='_blank'><xsl:value-of select="document($systemPath)//sml:PhysicalSystem/gml:name"/></a>
                </div>
            </xsl:if>
        </div>
    </xsl:template>
</xsl:stylesheet>