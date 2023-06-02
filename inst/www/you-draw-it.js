// !preview r2d3 data = data_to_json(list(line_data = tibble(x = seq(0, 20, .25), y = exp((x-15)/30)), point_data = tibble(x = seq(0, 20, length.out = 30), y = exp((x-15)/30 + rnorm(30, 0, 0.05))))), options = list(free_draw = FALSE, draw_start = 10, points_end = 15, pin_start = TRUE, x_range = c(0,20), y_range = c(.5,2), line_style = list(strokeWidth = 4), data_line_color = 'steelblue', drawn_line_color = 'steelblue', show_finished = TRUE, shiny_message_loc = 'my_shiny_app', linear = 'true', points = "partial", aspect_ratio = 1, x_by = 0.5), dependencies = c('d3-jetpack'),

// Make sure R has the following loaded
// library(tibble)
// data_to_json <- function(data) {jsonlite::toJSON(data, dataframe = "rows", auto_unbox = FALSE, rownames = TRUE)} 


// define variable system_font
 const system_font = `-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif, "Apple Color // // Emoji", "Segoe UI Emoji", "Segoe UI Symbol"`;


// ---------------------------------------------------------------------------------------------


// define variable margins
// if options.title == T then set top = 50, else top = 15 (see options list at top, currently, no title defined...)
const margin = {left: 55, 
                right: 10, 
                top: options.title ? 40: 10, 
                bottom: options.title? 25: 55};

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

// defines a changing variable called state??
// provides the data from top
// appends the svg group and moves it to the correct location...
// sets the width and height of the plot
// Since we are using object.assign, the options are passed in as the source, this will "overwrite" any information provided in the options???
// Is state like our wrapper?

let state = Object.assign({
  line_data: data.line_data,
  point_data: data.point_data,
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
  state.options = options;
  state.w = height*options.aspect_ratio;

  start_drawer(state);

});

// An explicit resize handler
// is this like our bounds??? or does this have to do with scales?
// https://rstudio.github.io/r2d3/articles/advanced_rendering.html
// redraws plot as you resize your browser window 
// (box has changed size that we did not do on code end)
r2d3.onResize(function(width, height, options) {
  state.w = height*state.options.aspect_ratio - margin.left - margin.right;
  state.h = height - margin.top - margin.bottom;
  
  start_drawer(state, reset = false);

});

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
      
      if(state.shiny_message_loc){
        // Make sure shiny is available before sending message
        if(typeof Shiny !== 'undefined'){
          // Send drawn points off to server
          Shiny.onInputChange(
            state.shiny_message_loc,
            state.drawable_points.map(d => d.y)
          );
        } else {
          console.log(`Sending message to ${state.shiny_message_loc}`);
        }
      }
    }
  };
  
  setup_draw_watcher(state.svg, scales, on_drag, on_end);
  
  // Do we have a title?
    if(state.title){
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

function setup_drawable_points({line_data, free_draw, draw_start}){
  if(free_draw){
    return line_data.map(d => ({x: d.x, y: null}));
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
  } else {
    var df = line_data
  }
  
  const finished_line = state.svg.selectAppend("path.finished_line")
  
    // Only draw line if there's something to draw.
  if(get_user_line_status(state) === 'unstarted'){
    finished_line.remove();
    return;
  }
  
  finished_line
  .datum(df)
  .at(default_line_attrs)
  .attr("d", scales.line_drawer)
  .attr("opacity", 0.5)
  
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

function add_axis_label(label, y_axis = true){
  
  const bump_axis = y_axis ? 'x': 'y';
  
  const axis_label_style = {
    [bump_axis]: bump_axis == 'y' ? 3: -2,
    textAnchor: 'end',
    fontWeight: '500',
    fontSize: '0.9rem',
  };
  
  return g => {
    let last_tick = g.select(".tick:last-of-type");
    const no_ticks = last_tick.empty();
    
    if(no_ticks){
      last_tick = g.append('g')
      .attr('class', 'tick');
    }
    
    last_tick.select("line").remove();
    
    last_tick.selectAppend("text")
    .at(axis_label_style)
    .html(label);
  };
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
    var y = d3.scaleLog()
    .domain(y_range || d3.extent(line_data, d => d.y))
    .range([h, 0]).base(10);
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