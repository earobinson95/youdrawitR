// !preview r2d3 data = data, options = options, dependencies = c('d3-jetpack'), d3_version = "5", viewer = "browser"

// Make sure R has the following loaded
// library(tibble)
// data_to_json <- function(data) {jsonlite::toJSON(data, dataframe = "rows", auto_unbox = FALSE, rownames = TRUE)} 


// define variable system_font
 const system_font = `-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif, "Apple Color // // Emoji", "Segoe UI Emoji", "Segoe UI Symbol"`;


// ---------------------------------------------------------------------------------------------


// define variable margins
// if x_lab and title true top = 30, if only title, top = 40, if only x top = 15, if neither top = 10.
const margin = {left: 55, 
                right: 10, 
               // top: options.title ? 40: 10, 
              //  bottom: options.title? 25: 55};
                top: (options.subtitle) ? 40 : (options.title && options.x_lab) ? 30 : (options.title ? 40 : (options.x_lab ? 15 : 10)),
                bottom: (options.subtitle) ? 30 : (options.title && options.x_lab) ? 35 : (options.title ? 25 : (options.x_lab ? 35 : 55))};

// define variable default line attributes
// do not fill the line in (default is filled in black beneath the line)
// stroke provides the color of the line stroke. This is either options.data_line_color (not yet defined) or steelblue
// sets the the stroke width (how thick)
// sets how tojoin the lines? and end the lines? rounded.
// Since we are using Object.assign, we are passing the source options.line_style into the vector of information defined. See top for options list.
const default_line_attrs = Object.assign({
  fill: "none",
  stroke: options.data_line_color || 'steelblue',
  strokeWidth: 4,
  strokeLinejoin: "round",
  strokeLinecap: "round",
}, options.line_style);

const conf_int_line_attrs = Object.assign({
  fill: "none",
  stroke: options.data_line_color || 'steelblue',
  strokeWidth: 2,
  strokeLinejoin: "round",
  strokeLinecap: "round",
  strokeDasharray: "10, 20"
}, options.line_style);

// defines a changing variable called state??
// provides the data from top
// appends the svg group and moves it to the correct location...
// sets the width and height of the plot
// Since we are using object.assign, the options are passed in as the source, this will "overwrite" any information provided in the options???
// Is state like our wrapper?

let state = Object.assign({
  line_data: data.line_data,
  point_data: data.point_data,
  lower_bound: data.lower_bound,
  upper_bound: data.upper_bound,
  svg: svg.append('g').translate([margin.left, margin.top]).attr("class", "wrapper"),
  w: height*options.aspect_ratio - margin.left - margin.right,
  h: height - margin.top - margin.bottom,
}, options);

// option variables are passed into state
// console.log(state.y_range)

// To distinguish between code that runs at initialization-time only and
// code that runs when data changes, organize your code so that the code
// which responds to data changes is contained within the r2d3.onRender()
// https://rstudio.github.io/r2d3/articles/advanced_rendering.html
r2d3.onRender(function(data, svg, width, height, options) {
  
  state.line_data = data.line_data;
  state.point_data = data.point_data;
  state = Object.assign(state, options);
  if (state.conf_int) {
    state.lower_bound = data.lower_bound;
    state.upper_bound = data.upper_bound;
  }
  state.options = options;
  state.w = height*options.aspect_ratio;

  start_drawer(state);
  
  if (!state.run_app) {
    // add download data buttons
  const downloadButton = svg.append("g")
    .attr("class", "button")
    .style("cursor", "pointer")
    .attr("transform", 
      `translate(${state.w + margin.right + margin.left}, ${margin.top})`)
    .on("click", handleDownloadClick);

  downloadButton.append("rect")
    .attr("x", 0)
    .attr("y", 0)
    .attr("width", 110)
    .attr("height", 25)
    .attr("rx", 5)
    .attr("ry", 5)
    .style("fill", "#ECECEC")
    .style("stroke", "black")
    .style("stroke-width", 2)
    
  downloadButton.on("mouseover", function() {
    d3.select(this).select("rect")
      .style("fill", "darkgray");
  })
  .on("mouseout", function() {
    d3.select(this).select("rect")
      .style("fill", "#ECECEC");
  });
  
  downloadButton.append("text")
    .attr("x", 55)
    .attr("y", 12.5)
    .attr("text-anchor", "middle")
    .attr("alignment-baseline", "middle")
    .attr("fill", "black")
    .attr("font-size", 14)
    .text("Download Data");
  
  function handleDownloadClick() {
    var drawn_line = svg.select("path.user_line").datum();
    var jsonData = JSON.stringify(drawn_line);
    var bb = new Blob([jsonData], { type: 'text/plain' });
    var a = document.createElement('a');
    a.download = 'draw_line_data.txt';
    a.href = window.URL.createObjectURL(bb);
    a.click();
  }
  }
});

// An explicit resize handler
// is this like our bounds??? or does this have to do with scales?
// https://rstudio.github.io/r2d3/articles/advanced_rendering.html
// redraws plot as you resize your browser window 
// (box has changed size that we did not do on code end)
r2d3.onResize(function(width, height, options) {
  state.w = height*state.options.aspect_ratio;
  state.h = height - margin.top - margin.bottom;

//  start_drawer(state, reset = false);

});

//function calculateDistance(user_line, line_data) {
//  let totalDistance = 0;
//  let pt_count = 0;
//
//  // Iterate over the points of user_line
//  for (let i = 0; i < user_line.length; i++) {
//    const point1 = user_line[i];
//    const correspondingPoint = line_data.find(point => point.x === point1.x);
//
//    if (correspondingPoint && correspondingPoint.y !== null) {
//      pt_count += 1
//      // Calculate the distance between the y-values of the two points
//      const distance = Math.abs(point1.y - correspondingPoint.y);
//      totalDistance += distance;
//    }
//  }
//  const averageDistance = totalDistance / pt_count;
//
//  return averageDistance;
//}

// JSON.stringify()
// Main function that draws current state of viz
// set up scales & draw true line if we decide to do that.
function start_drawer(state, reset = true){
  const scales = setup_scales(state);
  
  if(!state.free_draw){
    draw_true_line(state, scales, state.draw_start);
  }
  
  // Cover hidden portion with a rectangle
  // const line_hider = setup_line_hider(state.svg, state.draw_start, scales);
  
  // if we reset (these are points that can be drawn) remove what user has drawn.
  if(reset){
    // start with the svg object provided by r2d3
    // multiassign where setup_drawable_points is grabbing the parameters it needs from state: data, freedraw and draw_start
    state.drawable_points = setup_drawable_points(state);
  }
  
  // if we have points, we draw user's line.
  draw_user_line(state, scales);
  draw_rectangle(state, scales);
  draw_finished_line(state, scales, state.draw_start);
  
    // draw points for initial portion
  if(state.points != "none"){
    draw_points(state, scales);
  }
  
  // invert from pixle to data scale when they draw their points
  // THIS IS WHERE WE SET A SPECIFIC NUMBER OF POINTS THAT CAN BE DRAWN CORRESPONDING TO 1/2 UNITS ON DATA SCALE...MAKES IT CLUNKY: CONTROLED BY TIBBLE
  // ISSUES HERE!!!
  const on_drag = function(){
    const drag_x = scales.x.invert(d3.event.x);
    const drag_y = scales.y.invert(d3.event.y);
    fill_in_closest_point(state, drag_x, drag_y);
    draw_user_line(state, scales);
    draw_rectangle(state, scales);
  };
  
  // line_status is set by draw watcher - get user status line
  // if some points missing - in progress
  // complete line - done
  // passes drawn points to shiny when click done
  // as long as there are dots (missing spaces) it isn't going to pass data back.
  const on_end = function(){
    // Check if all points are drawn so we can reveal line
    const line_status = get_user_line_status(state);
    
    if(line_status === 'done'){
      // User has completed line drawing
      //if(state.show_finished) line_hider.reveal();
      //if(!state.free_draw)  line_hider.reveal();
      if(state.show_finished){
        draw_finished_line(state, scales, state.draw_start);
      }
//      const distance = calculateDistance(svg.select("path.user_line").datum(), state.line_data).toFixed(4);
//      console.log('Average distance between the drawn line and actual line: '+ distance);
      
      
      // Convert the completedLine to JSON
      if(typeof Shiny !== 'undefined') {
        var jsonData = JSON.stringify(svg.select("path.user_line").datum());
      
        // Send the data to the Shiny server
        Shiny.setInputValue("completedLineData", jsonData);
      }
      
      // Send the JSON data to R using an HTTP request
      // var request = new XMLHttpRequest();
    //  request.open("POST", "http://localhost:8000", true);
  //    request.setRequestHeader("Content-Type", "application/json");
//      request.send(jsonData);
      
    }
  };
  
    if(typeof Shiny !== 'undefined') {
    Shiny.addCustomMessageHandler('resetAction', function(reset) {
      paths.forEach(function(path) {
        path.remove();
      });
    
      // Reset paths and lineGens arrays
      paths = [];
      lineGens = [];
      
      if (isDrawing) {
        newLine();
      }
      start_drawer(state, reset);
    });
  }

  // Press button to be able to draw more lines
  var lineGen, path, data = [];
  var mousedown = false;
  var isDrawing = false;
  var draw_watcher = null;
  var paths = [];
  var lineGens = [];
  
  function newLine() {
    if (isDrawing) {
      // Stop drawing and remove new draw watcher
      isDrawing = false;
      buttonText.text("Draw Line");
      draw_watcher.remove();
  
      // Reattach the original event handlers
      svg.select('rect.drag_watcher').call(
        d3.drag()
        .on("drag", on_drag)
        .on("end", on_end)
      );
  
      return;
    }
  
    // Start drawing and create new draw watcher
    isDrawing = true;
    buttonText.text("Stop Drawing");
    
    // Detach the original event handlers
    svg.select('rect.drag_watcher').on(".drag", null);
  
    lineGen = d3.line()
      .x(function (d) { return d.x; })
      .y(function (d) { return d.y; });
    
    // Store the line generator
    lineGens.push(lineGen);
  
    path = state.svg.append('path')
      .at(conf_int_line_attrs)
      .attr("opacity", 0.75);
      
    // Store the path
    paths.push(path);
  
    data = [];  // clear data for new line
  
  draw_watcher = state.svg.append('rect')
    .attr('class', 'draw_watcher')
    .attr('height', scales.y.range()[0])
    .attr('width', scales.x.range()[1])
    .attr('fill', 'grey')
    .attr('fill-opacity', 0)
    .call(
      d3.drag()
      .on("start", function() {
        mousedown = true;
        var coords = d3.mouse(this);
        if (!coordsInWatcher(coords)) return;
        data.push({ x: coords[0], y: coords[1] });
        path.attr('d', lineGen(data));
      })
      .on("drag", function() {
        if (!mousedown) return;
        var coords = d3.mouse(this);
        if (!coordsInWatcher(coords)) return;
        data.push({ x: coords[0], y: coords[1] });
        path.attr('d', lineGen(data));
      })
      .on("end", function() {
        mousedown = false;
      })
    )
    .on('mouseout', function() {
      mousedown = false;
    });

  // Check if mouse is within draw_watcher
  function coordsInWatcher(coords) {
    var bounds = draw_watcher.node().getBBox();
    return coords[0] >= bounds.x && coords[0] <= bounds.x + bounds.width
      && coords[1] >= bounds.y && coords[1] <= bounds.y + bounds.height;
  }
  }
      const button = svg.append("g")
      .attr("class", "button")
      .style("cursor", "pointer")
      .attr("transform", `translate(${state.w + margin.right + margin.left}, ${margin.top + 75})`)
      .on("click", newLine);
    
    button.append("rect")
      .attr("x", 0)
      .attr("y", 0)
      .attr("width", 60)
      .attr("height", 25)
      .attr("rx", 5)
      .attr("ry", 5)
      .style("fill", "#ECECEC")
      .style("stroke", "black")
      .style("stroke-width", 2);
    
    button.on("mouseover", function() {
      d3.select(this).select("rect")
        .style("fill", "darkgray");
    })
    .on("mouseout", function() {
      d3.select(this).select("rect")
        .style("fill", "#ECECEC");
    });
    
    var buttonText = button.append("text")
      .attr("x", 30)
      .attr("y", 15)
      .attr("text-anchor", "middle")
      .attr("alignment-baseline", "middle")
      .attr("fill", "black")
      .attr("font-size", 14)
      .text("Draw Line");
      
  if (!state.run_app) {
    // add draw line and reset buttons
        const resetButton = svg.append("g")
      .attr("class", "button")
      .style("cursor", "pointer")
      .attr("transform", `translate(${state.w + margin.right + margin.left}, ${margin.top + 35})`)
      .on("click", handleClick)
    
    resetButton.append("rect")
      .attr("x", 0)
      .attr("y", 0)
      .attr("width", 60)
      .attr("height", 25)
      .attr("rx", 5)
      .attr("ry", 5)
      .style("fill", "#ECECEC")
      .style("stroke", "black")
      .style("stroke-width", 2);
      
    resetButton.on("mouseover", function() {
    d3.select(this).select("rect")
      .style("fill", "darkgray");
  })
  .on("mouseout", function() {
    d3.select(this).select("rect")
      .style("fill", "#ECECEC");
  });
    
    resetButton.append("text")
      .attr("x", 30)
      .attr("y", 15)
      .attr("text-anchor", "middle")
      .attr("alignment-baseline", "middle")
      .attr("fill", "black")
      .attr("font-size", 14)
      .text("Reset")
    
    // Click event handler
    function handleClick() {
      // Remove all new lines
      paths.forEach(function(path) {
        path.remove();
      });
    
      // Reset paths and lineGens arrays
      paths = [];
      lineGens = [];
      if (isDrawing) {
        newLine();
      }
      start_drawer(state, reset = true)
    }
  }
  
  setup_draw_watcher(state.svg, scales, on_drag, on_end);
  
  // Do we have a title?
    if(state.title){
      if (state.subtitle) {
        state.svg.append('text')
        .at({
          y: -margin.top/2 - 10,
          dominantBaseline: 'middle',
          fontSize: '1.3rem',
        })
        .style('font-family', system_font)
        .text(state.title);
      }
      else {
        state.svg.append('text')
        .at({
          y: -margin.top/2,
          dominantBaseline: 'middle',
          fontSize: '1.5rem',
        })
        .style('font-family', system_font)
        .text(state.title);
      }
    }
    
    // Do we have a subtitle?
    if (state.subtitle){
      state.svg.append('text')
      .at({
        y: -margin.top/2 + 10,
        dominantBaseline: 'middle',
        fontSize: '0.8rem',
      })
      .style('font-family', system_font)
      .text(state.subtitle);
    }
    
    // Do we have an x-axis label?
    if (state.x_lab) {
      if (state.subtitle) {
        state.svg.append('text')
        .at({
          x: state.w / 2,
          y: state.h + margin.bottom,
          fontSize: '0.8rem',
          textAnchor: 'middle',
        })
        .style('font-family', system_font)
        .text(state.x_lab);
      }
      else {
        state.svg.append('text')
        .at({
          x: state.w / 2,
          y: state.h + margin.bottom,
          fontSize: '1rem',
          textAnchor: 'middle',
        })
        .style('font-family', system_font)
        .text(state.x_lab);
      }
    }
    
    // Do we have a y-axis label?
    if (state.y_lab) {
      if (state.subtitle) {
        state.svg.append("text")
        .attr("transform", "rotate(-90)")
        .attr("x", 0 - (state.h / 2))
        .attr("y",  -margin.right - 22)
        .attr("font-size", "0.8rem")
        .attr("text-anchor", "middle")
        .style("font-family", system_font)
        .text(state.y_lab);
    }
      else {
      state.svg.append("text")
        .attr("transform", "rotate(-90)")
        .attr("x", 0 - (state.h / 2))
        .attr("y",  -margin.right - 22)
        .attr("font-size", "1rem")
        .attr("text-anchor", "middle")
        .style("font-family", system_font)
        .text(state.y_lab);
      }
    }
}

  function simplify_data({line_data, x_range, threshold_percentage}) {
    // simplify the drawable_points by binning the points based on a certain threshold_percentage
    // only allow 3 points per bin (when too many points in a cluster drawing line takes a while and 
    // if a large cluster can sometime be impossible to draw)
    const bin_size = x_range * threshold_percentage;
    const simplified_points = [];
    let current_bin = [];
  
    for (let i = 0; i < line_data.length; i++) {
      current_bin.push(line_data[i]);
      
      // if at last point or all points in bin x range have been added 
      if (i === line_data.length - 1 || line_data[i + 1].x - current_bin[0].x > bin_size) {
        // if there are more than 3 points in bin add the first, middle, and last point to drawable_points
        if (current_bin.length >= 3) {
          simplified_points.push(current_bin[0], current_bin[Math.floor(current_bin.length / 2)], current_bin[current_bin.length - 1]);
          // else add all points in current bin to drawable_points
        } else {
          simplified_points.push(...current_bin);
        }
        // reset current bin to empty
        current_bin = [];
      }
    }
    return simplified_points;
  }
  
  function interpolate_x({x_range, threshold_size, simplified_points}) {
    // Set threshold distance as % of the x-range
    const threshold_distance = x_range * threshold_size;
    
    // interpolate x value (adds drawble_points between extreme x values so the line does not jump)
    let drawable_points = [];

    for (let i = 0; i < simplified_points.length; i++) {
      const d = simplified_points[i];
      drawable_points.push(d);

      // If there is another point after this one and their X-distance is larger than 
      // the threshold value then we need to add interpolated points between them.
      if (i + 1 < simplified_points.length && Math.abs(simplified_points[i+1].x - d.x) > threshold_distance) {

        const interpolated_x = d3.range(d.x + threshold_distance, simplified_points[i+1].x, threshold_distance);

        interpolated_x.forEach((x,i) => {
          drawable_points.push({
            x: x,
            y: null
          });
        })
        
       }
     }
     //remove the repeated x values from drawable_points
     return drawable_points
}

  function setup_drawable_points({line_data, free_draw, draw_start}){
    if (free_draw) {
      if (state.x_range && (state.x_range[0] < line_data[0].x)) {
        line_data.unshift({ x: state.x_range[0], y: null });
      }
      if (state.x_range && state.x_range[1] > line_data[line_data.length - 1].x) {
       line_data.push({ x: state.x_range[1], y: null });
      }
      
      // Get range of x values from first and last point
      const x_range = line_data[line_data.length - 1].x - line_data[0].x
      
      let simplified_points = simplify_data({line_data, x_range, threshold_percentage: 0.05})
      
      let drawable_points = interpolate_x({x_range, threshold_size: 0.10, simplified_points})
      
      //remove the repeated x values from drawable_points
      return drawable_points
                .filter((d, i, arr) => i === 0 || d.x !== arr[i - 1].x)
                .map((d) => ({ x: d.x, y: null }));;
    } else {
      return line_data
      .filter(d => d.x >= draw_start)    
      .map((d,i) => ({
        x: d.x,
        y: i === 0 ? d.y: null
      }));
    }
  }

function get_user_line_status({drawable_points, free_draw}){
  const num_points = drawable_points.length;
  const num_filled = d3.sum(drawable_points.map(d => d.y === null ? 0: 1));
  const num_starting_filled = free_draw ? 0: 1;
  
  if(num_filled === num_starting_filled){
    return 'unstarted';
  } else if(num_points === num_filled){
    return 'done';
  } else {
    return 'in_progress';
  }
}

// Draw visable portion of line
function draw_true_line({svg, line_data, draw_start}, scales){
  var df = line_data.filter(function(d){ return d.x<=draw_start})
  state.svg.selectAppend("path.shown_line")
  .datum(df)
  .at(default_line_attrs)
  .attr("d", scales.line_drawer);
}

function draw_points({svg, point_data, points_end, points}, scales){
  
    if(points == "partial"){
      var df = point_data.filter(function(d){return (d.x<=points_end)});
    } else {
      var df = point_data;
    }
    
  const dots = state.svg.selectAll("circle").data(df)
  
  dots
    .enter().append("circle")
    .merge(dots)
    .attr("cx", d => scales.x(d.x))
    .attr("cy", d => scales.y(d.y))
    .attr("r", 2)
    .style("fill", "black")
    .style("opacity", 0.8)
    .style("stroke", "black")
    
}

if ((state.show_tooltip) || (typeof Shiny !== 'undefined')) {
  var tooltip = d3.select("body").append("div")
  .attr("class", "tooltip")
  .style("position", "absolute")
  .style("opacity", 0)
  .style("padding", "6px")
  .style("background-color", "#fff")
  .style("color", "#333")
  .style("border", "1px solid #ccc")
  .style("border-radius", "4px")
  .style("font-size", "14px")
  .style("pointer-events", "none");
}

if(typeof Shiny !== 'undefined') {
  // Recieve message from shiny to show or hide tooltip
  Shiny.addCustomMessageHandler("tooltipState", function(newState) {
    if (newState) {
      tooltip = d3.select("body").append("div")
        .attr("class", "tooltip")
        .style("position", "absolute")
        .style("opacity", 0)
        .style("padding", "6px")
        .style("background-color", "#fff")
        .style("color", "#333")
        .style("border", "1px solid #ccc")
        .style("border-radius", "4px")
        .style("font-size", "14px")
        .style("pointer-events", "none");
    }
    else {
      d3.select(".tooltip").remove();
    }
  });
}

function draw_rectangle({svg, drawable_points, line_data, draw_start, width, height, free_draw, x_by}, scales){
    if(get_user_line_status(state) === 'unstarted'){
      if(free_draw){
         var xmin = line_data[0].x
         var len  = line_data.length - 1
         var xmax = line_data[len].x
         var drawSpace_start = scales.x(xmin)
         var drawSpace_end   = scales.x(xmax)
       } else {
         var drawSpace_start = scales.x(draw_start)
         var drawSpace_end   = state.w
       }
    } else {
      if(get_user_line_status(state) === 'done'){
        var drawSpace_start = scales.x(1000000)
      } else {
        var df = drawable_points.filter(function(d){return (d.y === null)});
        var xmin = df[0].x - x_by
        var len  = line_data.length - 1
        var xmax = line_data[len].x
        var drawSpace_start = scales.x(xmin)
        var drawSpace_end   = scales.x(xmax)
      }
    }

    const draw_region = state.svg.selectAppend("rect");

    draw_region
      .attr("x", drawSpace_start)
      .attr("width",drawSpace_end - drawSpace_start)
      .attr("y", 0)
      .attr("height", state.h)
      //.style("fill", "#e0f3f3")
      .style("fill-opacity", 0.4)
      .style("fill", "rgba(255,255,0,.8)")

    if (typeof tooltip !== 'undefined') {
      svg.on("mousemove", function(d) {
      // Get the mouse coordinates relative to the SVG container
      var [mouseX, mouseY] = d3.mouse(this);
  
      // Check if the mouse is within the rectangle's bounds
      var isMouseOverDrawRegion =
        mouseX >= drawSpace_start &&
        mouseX <= drawSpace_end &&
        mouseY >= 0 &&
        mouseY <= state.h;
  
      if (isMouseOverDrawRegion) {
        // Calculate the progress based on the width of draw_region relative to the total width
        var progress = (1 - (drawSpace_end - drawSpace_start) / (drawSpace_end));
        
        // Set the progress to a minimum of 0 if it is negative
        progress = Math.max(progress, 0);
        
        // Show tooltip
        tooltip.transition()
          .duration(200)
          .style("opacity", 1);
        tooltip.html("Progress: " + (progress * 100).toFixed(2) + "%")
          .style("left", (d3.event.pageX + 10) + "px")
          .style("top", (d3.event.pageY - 28) + "px");
      } else {
        // Hide tooltip
        tooltip.transition()
          .duration(200)
          .style("opacity", 0);
      }
    });
    
    // Hide tooltip when mouse leaves the SVG container
    svg.on("mouseleave", function() {
      tooltip.transition()
        .duration(200)
        .style("opacity", 0);
    });
    }
}

function draw_user_line(state, scales){
  const {svg, drawable_points, drawn_line_color} = state;
  
  const user_line = state.svg.selectAppend("path.user_line");
  
  // Only draw line if there's something to draw.
  if(get_user_line_status(state) === 'unstarted'){
    user_line.remove();
    return;
  }

  // Draws the points the user is drawing with their mouse
  user_line
      .datum(drawable_points)
      .at(default_line_attrs)
      .attr('stroke', drawn_line_color)
      .attr("d", scales.line_drawer)
      .style("stroke-dasharray", ("1, 7"));
}

function draw_finished_line({svg, line_data, draw_start, free_draw}, scales){
  
  
  if(!free_draw){
    var df = line_data.filter(function(d){ return d.x >= draw_start})
    if (state.conf_int) {
      var lwr = state.lower_bound.filter(function(d){ return d.x >= draw_start})
      var upr = state.upper_bound.filter(function(d){ return d.x >= draw_start})
    }
  } else {
    var df = line_data
    if (state.conf_int) {
      var lwr = state.lower_bound
      var upr = state.upper_bound
    }
  }
  
  
  const finished_line = state.svg.selectAppend("path.finished_line")
  const lower_bound = state.svg.selectAppend("path.lower_bound")
  const upper_bound = state.svg.selectAppend("path.upper_bound")
  
    // Only draw line if there's something to draw.
  if(get_user_line_status(state) === 'unstarted'){
    finished_line.remove();
    if (state.conf_int) {
      lower_bound.remove();
      upper_bound.remove();
    }
    return;
  }
  
  finished_line
  .datum(df)
  .at(default_line_attrs)
  .attr("d", scales.line_drawer)
  .attr("opacity", 0.5)

  if (state.conf_int) {
    lower_bound
    .datum(lwr)
    .at(conf_int_line_attrs)
    .attr("d", scales.line_drawer)
    .attr("opacity", 0.5)
    
    upper_bound
    .datum(upr)
    .at(conf_int_line_attrs)
    .attr("d", scales.line_drawer)
    .attr("opacity", 0.5)
  }
  
}

// from state we need drawable_points - from setup_drawable_points() function that modifies state (get all x points bigger than or equal to draw_start and set up with a null), pin_start, and free_draw parameters
// drag_x, drag_y come from on_drag() function
function fill_in_closest_point({drawable_points, pin_start, free_draw}, drag_x, drag_y){
  // find closest point on data to draw
  let last_dist = Infinity;
  let current_dist;
  // If nothing triggers break statement than closest point is last point
  let closest_index = drawable_points.length - 1;
  const starting_index = free_draw ? 0 : (pin_start ? 1: 0);
  // for loop to check where closest point to where I am
  for(i = starting_index; i < drawable_points.length; i++){
    current_dist = Math.abs(drawable_points[i].x - drag_x);
    // If distances start going up we've passed the closest point
    if(last_dist - current_dist < 0) {
      closest_index = i - 1;
      break;
    }
    last_dist = current_dist;
  }
  
  drawable_points[closest_index].y = drag_y;
}


function setup_draw_watcher(svg, scales, on_drag, on_end){
  
  // could have called drag_watcher whatever we wanted
  // .at is space it begins watching
  // .call is "do something" d3.drag() is the mouse action puts listeners to react to user input.
  svg.selectAppend('rect.drag_watcher')
  .at({
    height: scales.y.range()[0],
    width: scales.x.range()[1],
    fill: 'grey',
    fillOpacity: 0,
  })
  .call(
    d3.drag()
    .on("drag", on_drag)
    .on("end", on_end)
  );
}

// Setup scales for visualization
function setup_scales(state){
  // multi-assign: x_range, etc. coming from options
  const {w, h, line_data, x_range, y_range, x_name, y_name, linear} = state;
  
  // convert x from data scale to pixle scale
  const x = d3.scaleLinear()
  .domain(x_range || d3.extent(line_data, d => d.x))
  .range([0, w]);
  
  //console.log(linear);
  if (linear == 'true') {
    //console.log('in linear block');
    // converts from data linear scale to pixle scale
    var y = d3.scaleLinear()
    .domain(y_range || d3.extent(line_data, d => d.y))
    .range([h, 0]);
  } else {
    //console.log('in log block');
    // converts from data log scale to pixle scale
    if (state.log_base == null) {
      var y = d3.scaleLog()
      .domain(y_range || d3.extent(line_data, d => d.y))
      .range([h, 0]).base(Math.E);
    }
    else {
      var y = d3.scaleLog()
      .domain(y_range || d3.extent(line_data, d => d.y))
      .range([h, 0]).base(state.log_base);
    }
  }
  
  const xAxis = d3.axisBottom().scale(x).tickSizeOuter(0);
  const yAxis = d3.axisLeft().scale(y).tickFormat(d3.format(".4")).tickSizeOuter(0);
  const xAxisGrid = d3.axisBottom().scale(x).tickSize(-h).tickFormat('');
  const yAxisGrid = d3.axisLeft().scale(y).tickSize(-w).tickFormat('');
  
  // Remove all grid related things first
  // remove everything when you convert from linear and log and vise versa.
  // want html element <g> that has class axis-grid
  state.svg.selectAll("g.x_grid").remove()
  state.svg.selectAll("g.y_grid").remove()
  // could call axis-grid "fred"
  state.svg.selectAll("g.axis-grid").remove()
  
  state.svg.selectAll("path.shown_line").remove()
  state.svg.selectAll("circle").remove()
  
  state.svg.selectAppend("g.x_grid")
  .attr('class', 'x axis-grid')
  .translate([0,h])
  .call(xAxisGrid);
  
  state.svg.selectAppend("g.y_grid")
  .attr('class', 'y axis-grid')
  .call(yAxisGrid);
  
  state.svg.selectAll(".axis-grid .tick")
  .style("stroke", "light-grey")
  .style("opacity", ".3");
  
  state.svg.selectAppend("g.x_axis")
  .translate([0,h])
  .call(xAxis);
  
  state.svg.selectAppend("g.y_axis")
  .call(yAxis);
  
  const line_drawer = d3.line()
  .defined(d => d.y !== null)
  .x(d => x(d.x))
  .y(d => y(d.y));
  
  return {
    x,
    y,
    line_drawer,
  };
}