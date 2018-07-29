import React, { Component } from 'react';
import Parameters from './components/parameters';
import Data from './components/data';
import './App.css';

//+'?'+Object.keys(params).map(k => k+'='+params[k]).join('&')
const http = (method, data, callback, error) => {
  fetch('http://localhost/api/'+method, {
      cache: 'no-cache',
      method: 'POST',
      // mode: 'cors',
      // headers: {
      //   'Accept': 'application/json',
      //   'Content-Type': 'application/json'
      // },
      body: JSON.stringify(data),
    })
    .then(res => Promise.all([res.ok, res.json()]))
    .then(([ok, data]) => (ok ? callback : error)(data))
    .catch(console.log);
}

class App extends Component {
  constructor() {
    super();
    this.state = {
      results: [],
      errors: [],
    };
    this.startSimulation = this.startSimulation.bind(this);
  }

  startSimulation(params) {
    http('simulate', params, data => {
      this.setState({results: data.results});
    }, data => {
      this.setState({errors: data.errors});
    })
  }

  render() {
    return (
      <div className="root">
        <Parameters
          startSimulation={this.startSimulation}
          errors={this.state.errors}
          />
        <Data
          results={this.state.results}
          />
      </div>
    );
  }
}

export default App;
