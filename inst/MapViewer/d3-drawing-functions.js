//  to build docs.  documentation build ./d3-drawing-functions.js -f html -o docs/codeDocs

/**
 *loads elements of the config file and parses, then runs all major functions
 *@param {object } error - returned from queue
 *@param {object }  config - json data loaded from the config file
 *@param {object } data - csv data loaded from the csv file
 */
function loadConfig(error, config, data){

    /** Set up config information
    */
    let choosen_scatter = config['choosen_scatter'];
    let numeric_vars = config['numeric_vars'];
    let id_vars =config['id_vars'];
    let time_vars = config['temporal_vars'];
    let layers = numeric_vars.concat(id_vars); //make this smarter to reduce by something
    let longitude_start = config['longitude_start'];
    let latitude_start = config['latitude_start'];
    let longitude_end = config['longitude_end'];
    let latitude_end = config['latitude_end'];
    let longitude_pt = config['longitude_pt'];
    let latitude_pt = config['latitude_pt'];
    let grid_file = config['grid_file'];
    let uniqueID = config['uniqueID']
    let access_token = config['mapbox_token'];

    let multi_grid = config['multi_grid'];
    let maps_available = multi_grid.map(a=>{return a['mapfile']});

    /** Create switch for points and lines to be drawn

    */
    let drawLines = false;
    let drawPt = false;

    if (typeof longitude_start !== 'undefined'
        & typeof latitude_start !== 'undefined'
        & typeof longitude_end !== 'undefined'
        & typeof latitude_end !== 'undefined'  ){
        drawLines = true;
    }

    if (typeof longitude_pt !== 'undefined'&
         typeof latitude_pt !== 'undefined'){
     drawPt = true;
     }
    /** Initatite Map simply
    */
    mapboxgl.accessToken = access_token;
    map = new mapboxgl.Map({
        container: 'map',
        style: 'mapbox://styles/mapbox/light-v9',
        zoom: 2,
        center: [-150.447, 45.753]
    });


    /** Create droppdown menus for choices
    */
    let mapdrop = document.getElementById('grid');
    mapdrop.length = 0;
    mapdrop.selectedIndex = 0;

    let map_option;
    for (let i = 0; i < maps_available.length; i++) {
      map_option = document.createElement('option');
      map_option.text = maps_available[i];
      map_option.value = maps_available[i];
      mapdrop.add(map_option);
    }



    //create dropdowns for all column variables
    let dropdown = document.getElementById('prop');
    dropdown.length = 0;
    dropdown.selectedIndex = 0;


    let option;
    for (let i = 0; i < layers.length; i++) {
      option = document.createElement('option');
      option.text = layers[i];
      option.value = layers[i]

      dropdown.add(option);
      if (layers[i]==choosen_scatter ){
          dropdown.selectedIndex = i;//set current scatter option
      }
    }

    /* Initate Legend Variables
    */
    let scatterLegend = legendInit('.scatterLegend');
    let zoneLegend = legendInit('.zoneLegend');
    let quantileScaleHist = d3.scaleQuantile();

    /*Initate Lower Bar Graphs
    */
    let svgInfo = initLowerBarChart('#viz');
    let svgInfoScatter = initLowerBarChart('#scatterviz');



    /** convert CSV data to geojson for Scatter Data
    * convert any positive longitudes to negative
    */
    let scatterLine =[];
    let scatterPt =[];

    data.forEach(function(d){

        if (drawLines){
            scatterLine.push(turf.lineString([[longConvert(Number(d[longitude_start])),Number(d[latitude_start])],[longConvert(Number(d[longitude_end])),Number(d[latitude_end])]], {UID: Number(d[uniqueID])},{id:Number(d[uniqueID])}));
            allScatterData =  turf.featureCollection(scatterLine);
        }
        if (drawPt){
            scatterPt.push(turf.point([longConvert(Number(d[longitude_pt])),Number(d[latitude_pt])], {UID: Number(d[uniqueID])},{id:Number(d[uniqueID])}));
            allScatterData =  turf.featureCollection(scatterPt);
        }
    })
    // allScatterData =  turf.featureCollection(scatterLine); // GEOJSON of CSV Data


    /** create SET of Areas used by data
    */
    const area_info = multi_grid.map(function(i) {
            let a = i['area_variable_column'];
            return data.map(function(d) {
                return Number(d[a])
            });
    })

    const area_set = area_info.map(i=>{return new Set(i)})

    let  zoneObjectAll = {}; //for all grids create dictionary of IDs and values
    for(let a=0; a<area_set.length; a++){
        let zoneObjectIndi = {};
                area_set[a].forEach(function(e){
                    zoneObjectIndi[e] = data.filter(function(d){
                        return Number(d[multi_grid[a]['area_variable_column']]) == e

                    })

                })
                zoneObjectAll[a]= zoneObjectIndi;

    }


    /* associate the colors for the legend to the scatter data and save values, color and ID's
    */
    let [scatterArray, scatterColor, temporalArray, num_or_id] = setScatterColor(data, numeric_vars, time_vars, id_vars, choosen_scatter, scatterLegend, uniqueID);

    /* paint the geojson layers of bathymetry, zones and scatter
    */
    makeTheMap(multi_grid, allScatterData);

    /* Add mouseover for scatter data to show info in right hand column and highlight data on lower bar graph
    */
    map.on("mousemove", "scatterLayer", function(e) {
            var scatter_left_info;

            if (e.features.length > 0) {

                if (hoveredStateId) {


                    map.setFeatureState({source: 'scatterLayer', id: hoveredStateId}, { hover: false});
                   d3.selectAll ("#id"+String(hoveredStateId)).attr("r", '2')// turn off scatter graph highlight
                    scatter_left_info = data.filter(function(d){
                    return d[uniqueID] == hoveredStateId
                   })

                }
                hoveredStateId = e.features[0].id;


                let allinfo = document.getElementById('allinfo');
                if (scatter_left_info){
                    allinfo.innerText = JSON.stringify(scatter_left_info[0], undefined, 2);
                }
                map.setFeatureState({source: 'scatterLayer', id: hoveredStateId}, { hover: true});
                d3.selectAll("#id"+String(hoveredStateId)).attr("r", '10')// turn off


            }
        });

    /* Set up callbacks for dropdowns
    */
    afterMapLoadsInit(data, choosen_scatter, uniqueID, numeric_vars, time_vars, id_vars, quantileScaleHist, scatterLegend, area_set, zoneLegend, svgInfo, svgInfoScatter, multi_grid, zoneObjectAll, drawLines, drawPt)

    /* Call starting values for map
    */
    map.on('load', function(e){
        //turn on grid visibility to inital grid and call inital functions
        let maplayer ='gridLayer' + String(0)
        map.setLayoutProperty(maplayer, 'visibility', 'visible');
        map.setLayoutProperty(maplayer+'Color', 'visibility', 'visible');
        zoneObject = zoneObjectAll[0]
        area_variable_map = multi_grid[0]['area_variable_map'];// for inital load use first grid only



        [scatterArray, scatterColor, temporalArray, num_or_id] = setScatterColor(data, numeric_vars, time_vars, id_vars, choosen_scatter, scatterLegend, uniqueID);
        if(drawLines){
            map.setPaintProperty('scatterLayer','line-color',['case',
                  ['has',['to-string', ['get', 'UID']],['literal',scatterColor]],
                  [ 'get',['to-string', ['get', 'UID']],['literal', scatterColor]],
                  "rgba(1,1,1,0)"
                ]
            )
         }
         if(drawPt){
            map.setPaintProperty('scatterLayer','circle-color',['case',
                  ['has',['to-string', ['get', 'UID']],['literal',scatterColor]],
                  [ 'get',['to-string', ['get', 'UID']],['literal', scatterColor]],
                  "rgba(1,1,1,0)"
                ]
            )
         }
    createDropDownGridInit(choosen_scatter, numeric_vars);
    resetzones(maplayer, choosen_scatter, "# of observations", area_set[0], quantileScaleHist, zoneLegend, svgInfo, area_variable_map)
    scatterPlot(svgInfoScatter, scatterArray, scatterColor, temporalArray, num_or_id)//TURN on for scatter
    })





}





/**
 based onthe choice in the zone dropdwon , calculates the aggregation choosen and associated the color for the zone

* @param {string} choosen_scatter - the variable that is being plotted as scatter
* @param {string} drop_down_hit - the value in the drop down menu choosen
* @param {Set} area_set - a Set object that has all the zones which have data
* @param {object} quantileScaleHist - d3.scaleQuantile object
* @param {object} zoneLegend - object that points to the legend for zones
* @returns {object}  the object with colors for each zone based on a histogram of data
*/
function setZoneHist(choosen_scatter, drop_down_hist, area_set, quantileScaleHist, zoneLegend){


    let zoneHistColor={};
    zoneHistInfo = {};// reset
    switch(drop_down_hist){
        case '# of observations':
            area_set.forEach(function(e){

                zoneHistInfo[e] = zoneObject[e].map(function(d){

                    return d[choosen_scatter]
                }).length;
            });
            break
        case '# of unique observations':
            area_set.forEach(function(e){

                zoneHistInfo[e] = new Set(zoneObject[e].map(function(d){

                    return d[choosen_scatter]
                })).size;
            });
            break
        case 'sum':
            area_set.forEach(function(e){

                zoneHistInfo[e] = d3.sum(zoneObject[e].map(function(d){

                    return Number(d[choosen_scatter])
                }));
            });
            break;
        case 'min':
            area_set.forEach(function(e){

                zoneHistInfo[e] = d3.min(zoneObject[e].map(function(d){
                    return Number(d[choosen_scatter])
                }));
            });
            break;
        case 'max':
            area_set.forEach(function(e){

                zoneHistInfo[e] = d3.max(zoneObject[e].map(function(d){

                    return Number(d[choosen_scatter])
                }));
            });
            break;
    }



    quantileScaleHist.domain(Object.values(zoneHistInfo))
    .range(['#f7f7f7',
            '#d9d9d9',
            '#bdbdbd',
            '#969696',
            '#737373',
            '#525252',
            '#252525']);


    area_set.forEach(function(d){
        zoneHistColor[d] = quantileScaleHist(zoneHistInfo[d]);

    })
    createLegend(quantileScaleHist, zoneLegend);

    return zoneHistColor
}

/**
* @param {object} data - columner data from csv file
* @param {object} numeric_vars - array of strings of the variables names that are numeric variables in the dataset
* @param {object} id_vars - array of strings of the variables names that are categorical variables in the dataset
* @param {string} choosen_scatter - the variable that is being plotted as scatter
* @param {object} scatterLegend - pointer to the legend for the scatter plot
* @param {string} uniqueID - the variable name pointing to the row id
* @returns {object}  the object with colors for each line of the scatter plot
*/
function setScatterColor(data, numeric_vars, time_vars, id_vars, choosen_scatter, scatterLegend, uniqueID){


    let scatterColor = {};
    let scatterArray = {};
    let scatterUnique = data.map(function(d){
            return d[uniqueID]

        });
    let temporalArray = data.map(function(d){
        return d[time_vars[0]]
    })
    let num_or_id = false;

    if (numeric_vars.includes(choosen_scatter)){
        scatterArray = data.map(function(d){
            return Number(d[choosen_scatter])

        })

         var quantileScaleScatter = d3.scaleQuantile();
        quantileScaleScatter
        .domain(scatterArray)
        .range(['#feebe2',
               '#fcc5c0',
                '#fa9fb5',
               '#f768a1',
                '#dd3497',
               '#ae017e',
                '#7a0177']);

        data.forEach(function(d) {
            scatterColor[d[uniqueID]] = quantileScaleScatter(Number(d[choosen_scatter]));
        });

        createLegend(quantileScaleScatter, scatterLegend);
        num_or_id = 'num';
    }
    else if (id_vars.includes(choosen_scatter)){
        scatterArray = data.map(function(d){
            return d[choosen_scatter]

        })
         // var ordinalScatter = d3.scaleOrdinal(d3.schemeCategory20c);
         var ordinalScatter = d3.scaleOrdinal();

        let ordinal_types = new Set(scatterArray);

        // ordinalScatter
        // .domain(ordinal_types) // change colors to based on set //FIXME
        // .range(['#feebe2',
        //        '#fcc5c0',
        //         '#fa9fb5',
        //        '#f768a1',
        //         '#dd3497',
        //        '#ae017e',
        //         '#7a0177']);

        ordinalScatter
        .domain(ordinal_types) // change colors to based on set //FIXME
        .range(['#a6cee3',
                '#1f78b4',
                '#b2df8a',
                '#33a02c',
                '#fb9a99',
                '#e31a1c',
                '#fdbf6f',
                '#ff7f00',
                '#cab2d6',
                '#6a3d9a',
                '#ffff99',
                '#b15928'])

        // ordinalScatter= d3.scaleSequential(d3.interpolateInferno)
        // .domain(ordinal_types)

        data.forEach(function(d) {
            scatterColor[d[uniqueID]] = ordinalScatter(d[choosen_scatter]);
        });

        createLegend(ordinalScatter, scatterLegend);
        num_or_id = 'id';

    }
     var prop = document.getElementById('prop');
    let scatterArrayID=Object.fromEntries(scatterUnique.map((_, i) => [scatterUnique[i], scatterArray[i]]))

    return [scatterArrayID, scatterColor, temporalArray, num_or_id]
}



/**
sets the colors of the zonal histograms and the associated barchart and legend

* @param {string} maplayer - the name of the Grid map choosen
* @param {string} choosen_scatter - the variable that is being plotted as scatter
* @param {string} choosen_grid_hist - the value in the drop down menu choosen
* @param {Set} area_set - a Set object that has all the zones whih have data
* @param {object} quantileScaleHist - d3.scaleQuantile object
* @param {object} zoneLegend - object that points to the legend for zones
* @param {object} svgInfo - svg that pojnts to the bar chart
* @global {object} map

**/
function resetzones(maplayer, choosen_scatter, choosen_grid_hist, area_set, quantileScaleHist, zoneLegend, svgInfo, area_variable_map){


        zoneHistColor = setZoneHist(choosen_scatter, choosen_grid_hist,  area_set, quantileScaleHist, zoneLegend);

        createLowerBarChart(quantileScaleHist,svgInfo);

        map.setPaintProperty(maplayer + 'Color',
            "fill-color",//["get",["to-string", ["get", "OBJECTID"]], ["literal", zoneColor]]
                ['case',
                  ['has',['to-string', ['get', area_variable_map]],['literal',zoneHistColor]],
                  [ 'get',['to-string', ['get', area_variable_map]],['literal', zoneHistColor]],
                  "rgba(1,1,1,0)"
                ]
            )
    }


/**
creates a listener for the choices in the dropdown for area as it is reset consitatnly

* @param {string} choosen_scatter - the variable that is being plotted as scatter
* @param {Set} area_set - a Set object that has all the zones whih have data
* @param {object} quantileScaleHist - d3.scaleQuantile object
* @param {object} zoneLegend - object that points to the legend for zones
* @param {object} svgInfo - svg that pojnts to the bar chart


**/


function createDropDownGridInit(choosen_scatter, numeric_vars){
    let dropdown = document.getElementById('gridType');
        dropdown.length = 0; //reset menu
        // dropdown.selectedIndex =0;
        if (numeric_vars.includes(choosen_scatter)){
                //create dropdowns for grouping choices
                let gridHistChoices = ['# of observations','# of unique observations','sum','min','max'];
                let option;
                for (let i = 0; i < gridHistChoices.length; i++) {
                  option = document.createElement('option');
                  option.text = gridHistChoices[i];
                  option.value = gridHistChoices[i]
                  dropdown.add(option);
                }
        }
        else{
                //create dropdowns for grouping choices
                let gridHistChoices = ['# of observations','# of unique observations'];
                let option;
                for (let i = 0; i < gridHistChoices.length; i++) {
                  option = document.createElement('option');
                  option.text = gridHistChoices[i];
                  option.value = gridHistChoices[i]
                  dropdown.add(option);

            }
        }
}


function create_source_draw_grid(gridLayer, gridinfo){
    map.addSource(gridLayer, {
            'type': 'geojson',
             'data':gridinfo['mapfile'],
             'tolerance':1.0,
             'buffer':0
        });


        map.addLayer({
            "id":gridLayer,
            "type":"fill",
            "source":gridLayer,
            "layout": {
                "visibility":"none"
            },
            "paint": {
                "fill-outline-color": "rgba(100,100,100,1)",
                "fill-color": "rgba(1,1,1,0.1)"
            }


        });

        map.addLayer({
            "id": gridLayer + 'Color',
            "type":"fill",
            "source":gridLayer,
            "layout": {
                "visibility":"none"
            },
            "paint": {
                "fill-outline-color": "rgba(1,1,1,0)",
                "fill-color":"transparent",
                "fill-opacity": 0.8,

            }


        });


     map.on("click", gridLayer + 'Color', function(e) {
            if (e.features.length > 0) {
                // if (hoveredStateId) {
                //    //console.log(zoneHistInfo[hoveredStateId] )
                // }

                var description = e.features[0].properties;

                new mapboxgl.Popup()
                    .setLngLat(e.lngLat)
                    .setHTML(JSON.stringify(description))
                    .addTo(map);

            }
        });

}

/**
set up the map with inital data, use basic colors for faster loading
@param {string} grid_file - the filename that contains the geojson grid file
@param {object} allScatterData -
@global {object} map
*/
function makeTheMap(multi_grid, allScatterData){


    map.on('load', function () {
        multi_grid.forEach((e, i) => {
            create_source_draw_grid('gridLayer'+String(i), e)

        })

        map.addSource('10m-bathymetry-81bsvj', {
            type: 'vector',
            url: 'mapbox://mapbox.9tm8dx88',
            'buffer':0
        });

        map.addLayer({
            "id": "10m-bathymetry-81bsvj",
            "type": "fill",
            "source": "10m-bathymetry-81bsvj",
            "source-layer": "10m-bathymetry-81bsvj",
            "layout": {},
            "paint": {
                "fill-outline-color": "hsla(337, 82%, 62%, 0)",
                // cubic bezier is a four point curve for smooth and precise styling
                // adjust the points to change the rate and intensity of interpolation
                "fill-color": [ "interpolate",
                    [ "cubic-bezier",
                        0, 0.5,
                        1, 0.5 ],
                    ["get", "DEPTH"],
                    0,  "#9fcbea",
                    9000, "#042f68"
                ]
            }
        }, 'barrier_line-land-polygon');

            if(allScatterData.features[0].geometry.type == 'LineString'){
                map.addLayer({
                    'id': 'scatterLayer',
                    'type': 'line',
                    'source': {
                       'type': 'geojson',
                    'data':allScatterData
                    },
                    "layout": {
                        "line-cap": "square"
                    },
                    'paint': {
                        'line-color':'white',
                        "line-width": ["case",
                        ["boolean", ["feature-state", "hover"], false],
                        5,
                        0.5
                        ]
                    },
                     'tolerance':1.0,
                     'buffer':0
                });
            }

            if(allScatterData.features[0].geometry.type == 'Point'){
                map.addLayer({
                    'id': 'scatterLayer',
                    'type': 'circle',
                    'source': {
                       'type': 'geojson',
                    'data':allScatterData
                    },
                    'paint': {
                        'circle-color':'white',
                        "circle-radius": ["case",
                        ["boolean", ["feature-state", "hover"], false],
                        7,
                        2.0
                        ]
                    },
                     'tolerance':1.0,
                     'buffer':0
                });
            }

            //show location of cursur in lower map corner
            map.on('mousemove', function (e) {
            document.getElementById('info').innerHTML =
            JSON.stringify(e.lngLat);


        })






    });

}


/**
creates inital map view state after styles load from mapbox
* @param {object} data
* @param {string} choosen_scatter - the variable that is being plotted as scatter
* @param {string} uniqueID - the string name of the variable representing the row ID for csv data
* @param {object} numeric_vars
* @param {object} id_vars
* @param {object} quantileScaleHist - d3.scaleQuantile object
* @param {object} scatterLegend - object that points to the legend for scatter dropdown
* @param {Set} area_set - a Set object that has all the zones whih have data
* @param {object} zoneLegend - object that points to the legend for zones
* @param {object} svgInfo - svg that pojnts to the bar chart
* @param {object} svgInfoScatter - svg that points to the scatter plot
* @param {object} multi_grid - the dictionary of maps avaialable to plot
* @param {zoneObjectAll} -  object contianing all the ID's to maps
* @param {boolean} drawLines- flag for using linear features
* @param {boolean} drawPy- flag for using point features
* @global {object} map

**/
function afterMapLoadsInit(data, choosen_scatter, uniqueID, numeric_vars, time_vars, id_vars, quantileScaleHist, scatterLegend, area_set, zoneLegend, svgInfo, svgInfoScatter, multi_grid, zoneObjectAll, drawLines, drawPt){
// after map loads
        let mapdrop = document.getElementById('grid');
        let gridHist = document.getElementById('gridType');

        mapdrop.addEventListener('change', function(){
            let mapChoice = this.selectedIndex;
            area_variable_map = multi_grid[mapChoice]['area_variable_map'];// for inital load use first grid only
            let maplayer = 'gridLayer' + String(mapChoice);

            for(let i = 0; i < this.length; i++) {
                if (i == this.selectedIndex){
                    map.setLayoutProperty('gridLayer' + String(i), 'visibility', 'visible');
                    map.setLayoutProperty('gridLayer' + String(i)+'Color', 'visibility', 'visible');
                } else{
                    map.setLayoutProperty('gridLayer' + String(i), 'visibility', 'none');
                    map.setLayoutProperty('gridLayer' + String(i)+'Color', 'visibility', 'none');
                }
            }
            zoneObject = zoneObjectAll[mapChoice];
            resetzones(maplayer, choosen_scatter, gridHist.value, area_set[mapChoice], quantileScaleHist, zoneLegend, svgInfo, area_variable_map)



        })




        gridHist.addEventListener('change', function() {
            let mapChoice = mapdrop.selectedIndex;
            area_variable_map = multi_grid[mapChoice]['area_variable_map'];// for inital load use first grid only
            let maplayer = 'gridLayer' + String(mapChoice);

            resetzones(maplayer, choosen_scatter, this.value, area_set[mapChoice], quantileScaleHist, zoneLegend, svgInfo, area_variable_map)

        });

         prop.addEventListener('change', function() {

            choosen_scatter = prop.value;

            let mapChoice = mapdrop.selectedIndex;
            area_variable_map = multi_grid[mapChoice]['area_variable_map'];// for inital load use first grid only
            let maplayer = 'gridLayer' + String(mapChoice);

            let [scatterArray, scatterColor, temporalArray, num_or_id] = setScatterColor(data, numeric_vars, time_vars, id_vars, choosen_scatter, scatterLegend, uniqueID);


            if(drawLines){
                map.setPaintProperty('scatterLayer','line-color',['case',
                      ['has',['to-string', ['get', 'UID']],['literal',scatterColor]],
                      [ 'get',['to-string', ['get', 'UID']],['literal', scatterColor]],
                      "rgba(1,1,1,0)"
                    ]
                )
             }
             if(drawPt){
                map.setPaintProperty('scatterLayer','circle-color',['case',
                      ['has',['to-string', ['get', 'UID']],['literal',scatterColor]],
                      [ 'get',['to-string', ['get', 'UID']],['literal', scatterColor]],
                      "rgba(1,1,1,0)"
                    ]
                )
             }
            createDropDownGridInit(choosen_scatter, numeric_vars);
            resetzones(maplayer, choosen_scatter, "# of observations", area_set[mapChoice], quantileScaleHist, zoneLegend, svgInfo, area_variable_map)
            scatterPlot(svgInfoScatter, scatterArray, scatterColor, temporalArray, num_or_id)//turn on for scatter

        })





}

/**
 *update lower bar chart with new histogram choices based on dropdowns
 *@param {object} colorsScale
 *@param {object} scgInfo
 *@global {object} zoneHistInfo
 */
function createLowerBarChart(colorsScale, svgInfo){

    let xgroup  = Object.keys(zoneHistInfo);
    let values = Object.values(zoneHistInfo);

    let someData = Object.keys(zoneHistInfo).map(function(key) {
      return [key, zoneHistInfo[key]];
    });
    // Setting the margin and dimensions of the work area

    // Creating the scale variables and setting the ranges
    var xScale = d3.scaleBand().rangeRound([0, svgInfo.width]).padding(0.1),
        yScale = d3.scaleLinear().rangeRound([svgInfo.height, 0]);

    // Adjusting data by assigning domain to the range of the scale
    xScale.domain(xgroup);
    yScale.domain([d3.min(values), d3.max(values)]);

    if(svgInfo){svgInfo.g.selectAll('g').remove()}

    // Appending X axis and formatting the text
    svgInfo.g.append('g')
        .attr('class', 'axisX')
        .attr('transform', 'translate(0,' + svgInfo.height + ')')
        .call(d3.axisBottom(xScale))
        // .attr('font-weight', 'bold')
        .selectAll("text")
        .style("text-anchor", "end")
        .attr("dx", "-.8em")
        .attr("dy", ".15em")
        .attr("transform", "rotate(-65)");



    // Appending Y axis
    svgInfo.g.append('g')
        .attr('class', 'axisY')
        .call(d3.axisLeft(yScale).ticks(10));

    // Creating chart
    let chart = svgInfo.g.selectAll('bar')
        .data(someData)
        .enter().append('g')

    // Appending bar chart to the chart
    chart.append('rect')
        .attr('class', 'bar')
        .attr('x', function(d) { return xScale(d[0]); })
        .attr('height', function(d) { return svgInfo.height - yScale(d[1]); })
        .attr('y', function(d) { return yScale(d[1]); })
        .attr('width', xScale.bandwidth())
        .attr('fill',function(d){return colorsScale(d[1])})

};


/**
 *updates the legend with the correct scale and colors
 *@param {object} scale - the d3 scale to update the colors
 *@param {object} svgVar - svg onject that is the legend
 */
function createLegend(scale, svgVar) {

  let legend = d3.legendColor()
    .labelFormat(d3.format(".02f"))
    //.cells(7)
    .scale(scale);


  svgVar.select(".legendQuant")
    .call(legend);
};

/**
 *inititilize the svg for the legend
 *@param {string} whichLegend - the class for the legend
 *@returns {object} the svg object of the legend
 */
function legendInit(whichLegend){


  let div = d3.select(whichLegend)



  let  svg = div.append("svg").attr("height","700");

  svg.append("g")
    .attr("class", "legendQuant")
    .attr("transform", "translate(20,20)")

  return svg
}

/**
 *initate the svg for the lower charts
 *@param {object} whcichViz - the svg info that points to the necessary graph
 */
function initLowerBarChart(whichViz){

    let chartHolder = document.getElementById(whichViz.substring(1));
    let margin = {top: 10, right: 50, bottom: 30, left: 50},
        width = chartHolder.clientWidth - margin.left - margin.right,
        height = 150 - margin.top - margin.bottom;

    let svgChart = d3.select(whichViz).selectAll('svg')
        .attr('width', width + margin.left + margin.right)
        .attr('height', height + margin.top + margin.bottom)


    let g = svgChart.append('g')
        .attr('transform', 'translate(' + margin.left + ',' + margin.top + ')');

    return {'g':g,'width':width,'height':height}
}



/** Createa a scatter plot of data
* @param(whichViz) - svg info of the chart initated
* @param(scatterArrayIds) - includes UniquesIDs as Keys and value for data
* @param(scatterColor) - includes UniquesIDs as Keys and color for data
*/
function scatterPlot(whichViz, scatterArrayIDs, scatterColor, temporalArray, num_or_id){
    let uniqueIDs = Object.keys(scatterArrayIDs);
    let allscat = Object.values(scatterArrayIDs);
    let zipData = uniqueIDs.map((e)=>{
        return {'color':scatterColor[e],'number':scatterArrayIDs[e],'id':e, 'time':temporalArray[e]}
    })

    let height = whichViz.height;

    let ids = Array.from(new Set(allscat));

    if (num_or_id == 'id'){
      var y = d3.scaleBand()
        .domain(ids)
        .range([height, 0]);

    }
    else{

         var y = d3.scaleLinear()
    .domain([d3.min(allscat), d3.max(allscat)])
    .range([ height, 0]);


    }


    var parseTime = d3.timeParse("%Y-%m-%d %H:%M:%S");
   // var parseTime = d3.timeParse("%y-%b-%d");
    var format = d3.timeFormat("%Y%m%d");




    var x = d3.scaleTime()
      .domain(d3.extent(temporalArray, function(d) { return parseTime(d); }))
      .range([ 0, whichViz.width ]);

   if(whichViz){whichViz.g.selectAll('g').remove()}

    // Appending X axis and formatting the text
    whichViz.g.append("g")
    .attr("transform", "translate(0," + height + ")")
    .call(d3.axisBottom(x));

  // Add Y axis

  whichViz.g.append("g")
    .call(d3.axisLeft(y));

  // Add dots
  whichViz.g.append('g')
    .selectAll("dot")
    .data(zipData)
    .enter()
    .append("circle")
      .attr("cx", function (d) { return x(parseTime(d.time)); } )
      .attr("cy", function (d) { return y(d.number); } )
      .attr("id", function(d){ return "id" + String(d.id); })
      .attr("r", 2)
      .style("fill", function(d){return d.color})



}

/** Convert any Longitudes that are poisitve to MOD -360
*/
function longConvert(x){
        if(x>0){
            return x-360
        }
        else{
            return x
        }
}

/** Toggle color layer of zonal histogram
*/
function gridLayerOff(){
    let gridLayerCheck = document.getElementById("gridLayerCheck");
    let mapdrop = document.getElementById('grid');
    let layer = 'gridLayer' + String(mapdrop.selectedIndex)+'Color';

  if (gridLayerCheck.checked == true){
    // find color map grid and turn on
    map.setLayoutProperty(layer, 'visibility', 'visible');
  } else {
     // find color map grid and turn on
     map.setLayoutProperty(layer, 'visibility', 'none');
  }
}

//P1
//TODO make time axis available to different time types
//add axis labels
//TODO add scatter graph for non numeric data
//FIX ORDINAL more then 5 colors

//P2
//TODO fix grid pop up
//todo connect click grid to lower graph

//P3
//add please wait while loading
// move points to new place
// filtering

//Done
//try big data
//set long data to 360 data
//toggle grid on/off
//time axis for y-m-d









