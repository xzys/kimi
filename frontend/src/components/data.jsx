import React, { Component } from 'react';
import classNames from 'classnames';
import update from 'immutability-helper';
import './data.css';

class Data extends Component {
  render() {
    return (
      <div className="results pane">
        <header>
          <div className="pane-title">Data</div>
        </header>
      </div>
    )
  }
}

export default Data;
