import React, { Component } from 'react';
import classNames from 'classnames';
import update from 'immutability-helper';
import './App.css';


const Reaction = (props) => {
  const {
    handleKeyPress,
    editGene,
    editReactants,
    editProducts,
    editRate,
    geneVisible,
  } = props;
  return (
    <div className="reaction-wrapper">
      <div className={classNames("gene", {visible: geneVisible})}>
        <input size={8} placeholder="gene"
          onKeyPress={handleKeyPress}
          onChange={editGene}/>
      </div>
      <div className="reaction">
        <input size={5} placeholder="reactants"
          onKeyPress={handleKeyPress}
          onChange={editReactants}/>
        <div className="reaction-arrow">{"\u2192"}</div>
        <input size={5} placeholder="products"
          onKeyPress={handleKeyPress}
          onChange={editProducts}/>
      </div>
      <div className="rate">
        <input type="number" placeholder="rate" step="any"
          onKeyPress={handleKeyPress}
          onChange={editRate}/>
      </div>
    </div>
  );
}

const emptyReaction = () => ({
  id: Math.random(),
  gene: '',
  reactants: '',
  products: '',
  rate: undefined,
});

class Reactions extends Component {
  constructor() {
    super();
    this.state = {
      reactions: [emptyReaction()],
    };
    this.handleKeyPress = this.handleKeyPress.bind(this);
    this.handleChange = this.handleChange.bind(this);
  }

  handleKeyPress(i, e) {
    if (e.key === 'Enter') {
      console.log('adding reaction at', i);
      this.setState({
        reactions: update(this.state.reactions, {$splice: [[i+1, 0, emptyReaction()]]})
      });
    }
  }

  handleChange(i, key, value) {
    this.setState({
      reactions: update(this.state.reactions, {[i]: {[key]: {$set: value}}})
    });
  }

  render() {
    const reactions = this.state.reactions.map((r, i) => (
      <Reaction key={r.id}
        handleKeyPress={e => this.handleKeyPress(i, e)}
        editGene={e => this.handleChange(i, 'gene', e.target.value)}
        editReactants={e => this.handleChange(i, 'reactants', e.target.value)}
        editProducts={e => this.handleChange(i, 'products', e.target.value)}
        editRate={e => this.handleChange(i, 'rate', e.target.value)}
        geneVisible={i > 0 ? r.gene && r.gene !== this.state.reactions[i-1].gene : true}
        reactants={r.reactants}
        products={r.products}
        rate={r.rate}
        />
    ));

    return (
      <div className="reaction-group">
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
          <div className="pane-label">Reactions</div>
          <Reactions/>
          <div className="pane-label">Molecules</div>
          <div className="pane-label">Signals</div>
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
