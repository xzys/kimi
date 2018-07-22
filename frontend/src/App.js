import React, { Component } from 'react';
import Parameters from './components/parameters';
import Data from './components/data';
import './App.css';

//+'?'+Object.keys(params).map(k => k+'='+params[k]).join('&')
const http = (method, data, callback, error) => {
  fetch('http://localhost:8000/'+method, {
      mode: 'no-cors',
      cache: 'no-cache',
      method: 'POST',
      body: JSON.stringify(data),
    })
    .then(res => res.status === 200 ? res.json() : error(res))
    .then(callback)
    .catch(error);
}

class App extends Component {
  constructor() {
    super();
    this.state = {
      data: undefined,
      errors: [],
    };
  }

  startSimulation(params) {
    http('simulate', params, data => {
      console.log(data); 
    }, err => {
      console.log(err);
    })
  }

  render() {
    return (
      <div className="root">
        <Parameters
          startSimulation={this.startSimulation}
          />
        <Data/>
      </div>
    );
  }
}

export default App;
