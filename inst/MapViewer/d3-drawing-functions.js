//  to build docs.  documentation build ./d3-drawing-functions.js -f html -o docs/codeDocs

/**
 *loads elements of the config file and parses, then runs all major functions
 *@param {object } error - returned from queue
 *@param {object }  config - json data loaded from the config file
 *@param {object } data - csv data loaded from the csv file
 */
function loadConfig(error, config, data){

    let area_variable_column = config['area_variable_column'];
    let area_variable_map = config['area_variable_map'];
    let choosen_scatter = config['choosen_scatter'];

    let numeric_vars = config['numeric_vars'];
    let id_vars =config['id_vars'];
    let time_vars = config['temporal_vars'];
    let layers = numeric_vars.concat(id_vars); //make this smarter to reduce by something
    let longitude_start = config['longitude_start'];
    let latitude_start = config['latitude_start'];
    let longitude_end = config['longitude_end'];
    let latitude_end = config['latitude_end'];
    let grid_file = config['grid_file'];
    let uniqueID = config['uniqueID']
    let access_token = config['mapbox_token'];

    mapboxgl.accessToken = access_token;
    map = new mapboxgl.Map({
        container: 'map',
        style: 'mapbox://styles/mapbox/light-v9',
        zoom: 2,
        center: [-150.447, 45.753]
    });




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
    }


    let scatterLegend = legendInit('.scatterLegend');
    let zoneLegend = legendInit('.zoneLegend');

    let columns = Object.keys(data[0]);

    let scatterData =[];

    let quantileScaleHist = d3.scaleQuantile();
    // var ordinalScatter = d3.scaleOrdinal();


    data.forEach(function(d){

       // scatterData.push(turf.lineString([[Number(d['LonLat_START_LON']),Number(d['LonLat_START_LAT'])],[Number(d['LonLat_END_LON']),Number(d['LonLat_END_LAT'])]], {UID: d['uniqueID'],'scatterValue':Number(d[choosen_scatter]),'scatterColor': quantileScaleScatter(Number(d[choosen_scatter]))}))
        scatterData.push(turf.lineString([[Number(d[longitude_start]),Number(d[latitude_start])],[Number(d[longitude_end]),Number(d[latitude_end])]], {UID: Number(d[uniqueID])},{id:Number(d[uniqueID])}));
    })
    allScatterData =  turf.featureCollection(scatterData);

    const area_info = data.map(function(d) {
            return Number(d[area_variable_column])
        });

    const area_set  = new Set(area_info);
    area_set.forEach(function(e){
        zoneObject[e] = data.filter(function(d){
            return Number(d[area_variable_column]) == e

        })

    })
    let scatterColor = setScatterColor(data, numeric_vars, choosen_scatter, scatterLegend, uniqueID);
    // map.once('style.load', function(e) {

    makeTheMap(grid_file, allScatterData);

    map.on("mousemove", "scatterLayer", function(e) {
            var scatter_left_info;

            if (e.features.length > 0) {

                if (hoveredStateId) {


                    map.setFeatureState({source: 'scatterLayer', id: hoveredStateId}, { hover: false});
                    scatter_left_info = data.filter(function(d){
                    return d[uniqueID] == hoveredStateId
                   })
                }
                hoveredStateId = e.features[0].id;


                let allinfo = document.getElementById('allinfo');
                allinfo.innerText = JSON.stringify(scatter_left_info[0], undefined, 2);
                map.setFeatureState({source: 'scatterLayer', id: hoveredStateId}, { hover: true});


            }
        });
    map.on("click", "GridLayerColor", function(e) {
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




    // map.once('style.load', function(e) {
    let svgInfo = initLowerBarChart();
    afterMapLoadsInit(data, choosen_scatter, uniqueID, numeric_vars, id_vars, quantileScaleHist, scatterLegend, area_set, zoneLegend, svgInfo, area_variable_map)
    // })
    // resetzones(choosen_scatter, "# of observations", area_set, quantileScaleHist, zoneLegend, svgInfo, map)
     // map.on('styledata', function(e){
    map.on('load', function(e){
        scatterColor = setScatterColor(data, numeric_vars, id_vars, choosen_scatter, scatterLegend, uniqueID);

        map.setPaintProperty('scatterLayer','line-color',['case',
              ['has',['to-string', ['get', 'UID']],['literal',scatterColor]],
              [ 'get',['to-string', ['get', 'UID']],['literal', scatterColor]],
              "rgba(1,1,1,0)"
            ]
        )
    createDropDownGridInit(choosen_scatter, numeric_vars, area_set, quantileScaleHist, zoneLegend, svgInfo, map);
    resetzones(choosen_scatter, "# of observations", area_set, quantileScaleHist, zoneLegend, svgInfo, area_variable_map)
    })





}





/**
* @param {string} choosen_scatter - the variable that is being plotted as scatter
* @param {string} drop_down_hit - the value in the drop down menu choosen
* @param {Set} area_set - a Set object that has all the zones which have data
* @param {object} quantileScaleHist - d3.scaleQuantile object
* @param {object} zoneLegend - object that points to the legend for zones
* @returns {object}  the object with colors for each zone based on a histogram of data
*/
function setZoneHist(choosen_scatter, drop_down_hist, area_set, quantileScaleHist, zoneLegend){


    let zoneHistColor={};
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
    createLegend(quantileScaleHist,zoneLegend);

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
function setScatterColor(data, numeric_vars, id_vars, choosen_scatter, scatterLegend, uniqueID){


    let scatterColor ={};
    if (numeric_vars.includes(choosen_scatter)){
        let scatter_array = data.map(function(d){
            return Number(d[choosen_scatter])

        })
         var quantileScaleScatter = d3.scaleQuantile();
        quantileScaleScatter
        .domain(scatter_array)
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
    }
    else if (id_vars.includes(choosen_scatter)){
        let scatter_array = data.map(function(d){
            return d[choosen_scatter]

        })
        var ordinalScatter = d3.scaleOrdinal();

        let ordinal_types = new Set(scatter_array);

        ordinalScatter
        .domain(scatter_array) // change colors to based on set
        .range(['#feebe2',
               '#fcc5c0',
                '#fa9fb5',
               '#f768a1',
                '#dd3497',
               '#ae017e',
                '#7a0177']);
        data.forEach(function(d) {
            scatterColor[d[uniqueID]] = ordinalScatter(d[choosen_scatter]);
        });

        createLegend(ordinalScatter, scatterLegend);

    }
     var prop = document.getElementById('prop');

    return scatterColor;
}



/**
sets the colors of the zonal histograms and the associated barchart and legend

* @param {string} choosen_scatter - the variable that is being plotted as scatter
* @param {string} choosen_grid_hist - the value in the drop down menu choosen
* @param {Set} area_set - a Set object that has all the zones whih have data
* @param {object} quantileScaleHist - d3.scaleQuantile object
* @param {object} zoneLegend - object that points to the legend for zones
* @param {object} svgInfo - svg that pojnts to the bar chart
* @global {object} map

**/
function resetzones(choosen_scatter, choosen_grid_hist, area_set, quantileScaleHist, zoneLegend, svgInfo, area_variable_map){


        zoneHistColor = setZoneHist(choosen_scatter, choosen_grid_hist, area_set, quantileScaleHist, zoneLegend);

        createLowerBarChart(quantileScaleHist,svgInfo);

        map.setPaintProperty('GridLayerColor',
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
function createGridDropDownCallback(choosen_scatter, area_set, quantileScaleHist, zoneLegend, svgInfo){

    let gridHist = document.getElementById('gridType');

    gridHist.addEventListener('change', function() {

        resetzones(choosen_scatter, this.value, area_set, quantileScaleHist, zoneLegend, svgInfo)

    });
}

function createDropDownGridInit(choosen_scatter, numeric_vars, area_set, quantileScaleHist, zoneLegend, svgInfo){
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

/**
set up the map with inital data, use basic colors for faster loading
@param {string} grid_file - the filename that contains the geojson grid file
@param {object} allScatterData -
@global {object} map
*/
function makeTheMap(grid_file, allScatterData){


    map.on('load', function () {
        map.addSource('statsArea', {
            'type': 'geojson',
             'data':grid_file,
             'tolerance':1.0,
             'buffer':0
        });

        // map.addSource('statsArea', {

        //           "type": "vector",
        //           "tiles": ["http://localhost:8080/data/Groundfish_Statistical_Areas_2001/{z}/{x}/{y}.pbf"],
        //           'minzoom': 1,
        //             'maxzoom': 17


        // });

        map.addLayer({
            "id":'GridLayer',
            "type":"fill",
            "source":'statsArea',
            // "source-layer":'Groundfish_Statistical_Areas_2001',
            "layout": {},
            "paint": {
                "fill-outline-color": "rgba(100,100,100,1)",
                "fill-color": "rgba(1,1,1,0.1)"
            }


        });

        map.addLayer({
            "id":'GridLayerColor',
            "type":"fill",
            "source":'statsArea',
            // "source-layer":'Groundfish_Statistical_Areas_2001',
            "layout": {},
            "paint": {
                "fill-outline-color": "rgba(1,1,1,0)",
                "fill-color":"transparent",
                // "fill-color": //["get",["to-string", ["get", "OBJECTID"]], ["literal", zoneColor]]
                //     ['case',
                //       ['has',['to-string', ['get', 'NMFS_AREA']],['literal',zoneHistColor]],
                //       [ 'get',['to-string', ['get', 'NMFS_AREA']],['literal', zoneHistColor]],
                //       "rgba(1,1,1,0)"
                //     ],
                // "fill-opacity": ["case",
                //     ["boolean", ["feature-state", "hover"], false],
                //     1,
                //     0.5
                // ]
                "fill-opacity": 0.8,

            }


        });

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


            // map.addSource('scatterLayer', {

            //       "type": "vector",
            //       "tiles": ["http://localhost:8080/data/scatter/{z}/{x}/{y}.pbf"],
            //       'minzoom': 1,
            //         'maxzoom': 17
            //     })
            // map.addLayer({
            //     'id': 'scatterLayer',
            //     'type': 'line',
            //     'source': 'scatterLayer',
            //     'source-layer':'scatterLayer',
            //     "layout": {
            //         "line-cap": "square"
            //     },
            //     'paint': {
            //         'line-color':'white',
            //         "line-width": ["case",
            //         ["boolean", ["feature-state", "hover"], false],
            //         5,
            //         0.5
            //         ]
            //            }
            //     // },
            //     //  'tolerance':1.0,
            //     //  'buffer':0
            // });

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
                     // 'line-color':
                     //     ['get', 'scatterColor']
                    //  'line-color':
                    //  ['case',
                    //   ['has',['to-string', ['get', 'UID']],['literal',scatterColor]],
                    //   [ 'get',['to-string', ['get', 'UID']],['literal', scatterColor]],
                    //   "rgba(1,1,1,0)"
                    // ],
                    "line-width": ["case",
                    ["boolean", ["feature-state", "hover"], false],
                    5,
                    0.5
                    ]
                },
                 'tolerance':1.0,
                 'buffer':0
            });
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
* @global {object} map

**/
function afterMapLoadsInit(data, choosen_scatter, uniqueID, numeric_vars, id_vars, quantileScaleHist, scatterLegend, area_set, zoneLegend, svgInfo, area_variable_map){
// after map loads


        let gridHist = document.getElementById('gridType');

        gridHist.addEventListener('change', function() {

            resetzones(choosen_scatter, this.value, area_set, quantileScaleHist, zoneLegend, svgInfo, area_variable_map)

        });

         prop.addEventListener('change', function() {

            choosen_scatter = prop.value;

            let scatterColor = setScatterColor(data, numeric_vars, id_vars, choosen_scatter, scatterLegend, uniqueID);

            map.setPaintProperty('scatterLayer','line-color',['case',
                  ['has',['to-string', ['get', 'UID']],['literal',scatterColor]],
                  [ 'get',['to-string', ['get', 'UID']],['literal', scatterColor]],
                  "rgba(1,1,1,0)"
                ]
            )
            createDropDownGridInit(choosen_scatter, numeric_vars, area_set, quantileScaleHist, zoneLegend, svgInfo);
            resetzones(choosen_scatter, "# of observations", area_set, quantileScaleHist, zoneLegend, svgInfo, area_variable_map)


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
    yScale.domain([0, d3.max(values)]);

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


  let div = d3.select(whichLegend).append("div")
    .attr("class", "column");


  let  svg = div.append("svg");

  svg.append("g")
    .attr("class", "legendQuant")
    .attr("transform", "translate(20,20)");

  return svg
}

/**
 *initate the svg for the bar chart that shows the spatial histogram
 */
function initLowerBarChart(){


    let margin = {top: 50, right: 30, bottom: 30, left: 50},
        width = 700 - margin.left - margin.right,
        height = 200 - margin.top - margin.bottom;

    let svgChart = d3.select('#viz').selectAll('svg')
        .attr('width', width + margin.left + margin.right)
        .attr('height', height + margin.top + margin.bottom)


    let g = svgChart.append('g')
        .attr('transform', 'translate(' + margin.left + ',' + margin.top + ')');

    return {'g':g,'width':width,'height':height}
}

function sum(a,b){
 return a+b;
}


