import React, { Component } from 'react';
import classNames from 'classnames';
import update from 'immutability-helper';
import * as d3 from 'd3';
import './data.css';

window.d3 = d3;

class Graphs extends Component {
  componentDidUpdate() {
    const { results } = this.props;
    console.log(results);
    const [maxX, maxY]  = results.reduce(
      (acc, r) => r.timeseries.reduce(
        (acc, x) => [Math.max(acc[0], ...x.times), 
                     Math.max(acc[1], ...[].concat(...Object.values(x.molecules)))], 
        acc),
      [0, 0]);
    const [minX, minY]  = results.reduce(
      (acc, r) => r.timeseries.reduce(
        (acc, x) => [Math.min(acc[0], ...x.times), 
                     Math.min(acc[1], ...[].concat(...Object.values(x.molecules)))], 
        acc),
      [maxX, maxY]);
    
    console.log(minX, maxX, minY, maxY);
    const margin = {top: 10, bottom: 20, left: 25, right: 10},
          width = 600 - margin.right - margin.left,
          height = 300 - margin.top - margin.bottom;

    const x = d3.scaleLinear()
                .domain([minX, maxX])
                .range([0, width]);
    const y = d3.scaleLinear()
                .domain([0, maxY])
                .range([height, 0]);
    const xAxis = d3.axisBottom(x),
          yAxis = d3.axisLeft(y);

    const graphs = d3.select(this.root).selectAll('svg')
      .data(results)
      .enter().append('div')
        .attr('class', 'graph')
        .append('svg')
          .attr("preserveAspectRatio", "xMinYMin meet")
          .attr("viewBox", "0 0 "+(width + margin.left + margin.right)+" "+(height + margin.top + margin.bottom))
          .append('g')
            .attr('transform', 'translate('+margin.left+','+margin.top+')');
    graphs.append("g")
      .attr("class", "axis axis-x")
      .attr("transform", "translate(0," + height + ")")
      .call(xAxis);
    graphs.append("g")
      .attr("class", "axis axis-y")
      .call(yAxis)
    graphs.exit().remove();

    const lineExtractor = mol => series => d3.line()
      .x((d, i) => x(series.times[i]))
      .y((d, i) => y(series.molecules[mol][i]))
      (Array(series.times.length));

    const mols = results ? Object.keys(results[0].timeseries[0].molecules) : [];
    const lines = mols.reduce(
      (acc, m) => {acc[m] = lineExtractor(m); return acc},
      {});

    const timeseries = graphs.selectAll('.series')
      .data(result => result.timeseries)
      .enter().append('g')
        .attr('class', 'series');

    const colorScale = d3.scaleOrdinal(d3.schemeSet1);
    const molecules = timeseries.selectAll('.molecules')
      .data(series => Object.keys(series.molecules).map(m => ({series, m})))
      .enter().append('path')
        .attr('d', d => lines[d.m](d.series))
        .attr('stroke', (d, i) => colorScale(i))
  }

  render() {
    return (
      <div className="graphs" ref={c => this.root = c}></div>
    );
  }
}

class Data extends Component {
  render() {
    const { results } = this.props;
    return (
      <div className="results pane">
        <header>
          <div className="pane-title">Data</div>
        </header>
        <div className="pane-content">
          <Graphs results={results}
            />
        </div>
      </div>
    )
  }
}

export default Data;
