import React, { Component } from 'react';
import './App.css';


class Reaction extends Component {
  handleKeyPress(e) {
    if (e.key === 'Enter') {
      this.props.addNewReaction();
    }
  }

  render() {
    return (
      <div className="reaction-wrapper">
        <div className="reaction">
          <input size={5} placeholder="reactants" onKeyPress={this.handleKeyPress.bind(this)}/>
          <div className="reaction-arrow">{"\u2192"}</div>
          <input size={5} placeholder="products" onKeyPress={this.handleKeyPress.bind(this)}/>
        </div>
        <div className="rate">
          <input type="number" placeholder="rate" onKeyPress={this.handleKeyPress.bind(this)}/>
        </div>
      </div>
    );
  }
}

class ReactionGroup extends Component {
  constructor() {
    super();
    this.state = {
      reactions: [],
    };
  }

  addNewReaction() {
    console.log('New Reaction!');
  }

  render() {
    const reactions = false ? (
        []
      ) : (
        <Reaction/>
      );
    return (
      <div className="reaction-group">
        <input size={8} className="gene" type="text" placeholder="gene"/>
        <div className="reactions">
          {reactions}
        </div>
      </div>
    )
  }
}


class Parameters extends Component {
  render() {
    return (
      <div className="parameters pane">
        <header>
          <div className="pane-title">Parameters</div>
        </header>
        <div className="pane-content">
          <div className="pane-subtitle">Reactions</div>
          <ReactionGroup/>
        </div>
      </div>
    )
  }
}


class Results extends Component {
  render() {
    return (
      <div className="results pane">
        <header>
          <div className="pane-title">Results</div>
        </header>
      </div>
    )
  }
}


class App extends Component {
  render() {
    return (
      <div className="root">
        <Parameters/>
        <Results/>
      </div>
    );
  }
}

export default App;
