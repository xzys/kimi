import React, { Component } from 'react';
import Parameters from './components/parameters';
import Data from './components/data';
import './App.css';

class App extends Component {
  render() {
    return (
      <div className="root">
        <Parameters/>
        <Data/>
      </div>
    );
  }
}

export default App;
