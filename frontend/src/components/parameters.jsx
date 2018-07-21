import React, { Component } from 'react';
import classNames from 'classnames';
import update from 'immutability-helper';
import './parameters.css';

/* Reactions */

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
        <span className="reaction-arrow">{"\u2192"}</span>
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

const Reactions = (props) => {
  const { reactions, handleKeyPress, handleChange } = props;
  const reactions_ = reactions.map((r, i) => (
    <Reaction key={r.id}
      handleKeyPress={e => handleKeyPress(i, e)}
      editGene={e => handleChange(i, 'gene', e.target.value)}
      editReactants={e => handleChange(i, 'reactants', e.target.value)}
      editProducts={e => handleChange(i, 'products', e.target.value)}
      editRate={e => handleChange(i, 'rate', e.target.value)}
      geneVisible={i > 0 ? r.gene && r.gene !== reactions[i-1].gene : true}
      reactants={r.reactants}
      products={r.products}
      rate={r.rate}
      />
  ));
  return (
    <div className="reactions">
      {reactions_}
    </div>
  );
}

/* Signals */

const Signal = (props) => {
  const { signal, value, editSignal, editValue, handleKeyPress } = props;
  return (
    <div className="signal-wrapper">
      <div className="signal">
        <input size={8} placeholder="signal"
          onKeyPress={handleKeyPress}
          onChange={editSignal}
          />
      </div>
      <span className="signal-equals">{"\u003D"}</span>
      <div className="signal-value">
        <input placeholder="value" type="number"
          onKeyPress={handleKeyPress}
          onChange={editValue}
          />
      </div>
    </div>
  )
}

const SignalSet = (props) => {
  const { index, name, signals, handleKeyPress, handleChange } = props;
  const signals_ = signals.map((s, i) => (
    <Signal key={s.id}
      handleKeyPress={e => handleKeyPress(index, i, e)}
      editSignal={e => handleChange(index, i, 'signal', e.target.value)}
      editValue={e => handleChange(index, i, 'value', e.target.value)}
      />
  ))
  return (
    <div className="signal-set">
      <input disabled className="signal-set-name" value={name}/>
      <div className="signals">
        {signals_}
      </div>
    </div>
  );
}

const Signals = (props) => {
  const { signalsets, handleKeyPress, handleChange } = props;
  const signalSets = signalsets.map((ss, i) => (
    <SignalSet key={ss.id} index={i} name={ss.name} signals={ss.signals}
      handleKeyPress={handleKeyPress}
      handleChange={handleChange}
      />
  ));
  return (
    <div className="signal-sets">
      {signalSets}
    </div>
  );
}

/* Molecules */

const Molecule = (props) => {
  const { molecule, count, handleChange } = props;
  return (
    <div className="molecule-wrapper">
      <div className="molecule">
        <input size={12} placeholder="molecule" disabled
          value={molecule}/>
      </div>
      <span className="molecule-equals">{"\u003D"}</span>
      <div className="molecule-count">
        <input size={5} placeholder="count" type="number"
          onChange={handleChange}
          value={count}/>
      </div>
    </div>
  );
}

const Molecules = (props) => {
  const { molecules, editCount } = props;
  const molecules_ = Object.keys(molecules).map((m, i) => (
    <Molecule key={i} molecule={m} count={molecules[m]}
      handleChange={e => editCount(m, e.target.value)}
      />
  ));
  return (
    <div className="molecules">
      {molecules_.length ? molecules_ : 
				<div className="placeholder">
					This section will be populated from the reactions above.
				</div>
			}
    </div>
  );
}

/* Parameters */

const emptyReaction = () => ({
  id: Math.random(),
  gene: '',
  reactants: '',
  products: '',
  rate: undefined,
});


const emptySignal = () => ({
  id: Math.random(),
  signal: "",
  value: 0,
});

const emptySignalSet = (name) => ({
  id: Math.random(),
  name: name,
  signals: [emptySignal()]
});

const parseExpression = s => s.split('+').map(s => s.trim()).filter(Boolean);

class Parameters extends Component {
  constructor() {
    super();
    this.state = {
      reactions: [emptyReaction()],
      molecules: {},
      signalsets: [
        emptySignalSet('Signals A'),
        emptySignalSet('Signals B'),
      ]
    };
    this.reactionsHandleKeyPress = this.reactionsHandleKeyPress.bind(this);
    this.reactionsHandleChange = this.reactionsHandleChange.bind(this);
    this.moleculesEditCount = this.moleculesEditCount.bind(this);
    this.signalsHandleKeyPress = this.signalsHandleKeyPress.bind(this);
    this.signalsHandleChange = this.signalsHandleChange.bind(this);
  }

  reactionsHandleKeyPress(i, e) {
    if (e.key === 'Enter') {
      this.setState({
        reactions: update(this.state.reactions, {$splice: [[i+1, 0, emptyReaction()]]})
      });
    }
  }

  reactionsHandleChange(i, key, value) {
    const newReactions = update(this.state.reactions, {[i]: {[key]: {$set: value}}});
    const allMolecules = newReactions.reduce(
      (acc, r) => acc.concat(parseExpression(r.reactants), parseExpression(r.products)),
      []);
    const newMolecules = allMolecules.reduce((acc, m) => {
      acc[m] = this.state.molecules[m] || 0;
      return acc;
    }, {});
    this.setState({
      reactions: newReactions,
      molecules: newMolecules
    });
  }

  moleculesEditCount(m, count) {
    this.setState({molecules: update(this.state.molecules, {[m]: {$set: count}})});
  }

  signalsHandleKeyPress(signalsetIndex, signalIndex, e) {
    if (e.key == 'Enter') {
      this.setState({
        signalsets: update(this.state.signalsets, 
          {[signalsetIndex]: {signals: {$splice: [[signalIndex+1, 0, emptySignal()]]}}})
      });
    }
  }

  signalsHandleChange(signalsetIndex, signalIndex, field, value) {
    console.log(signalsetIndex, signalIndex, field, value);
    this.setState({
      signalsets: update(this.state.signalsets, 
        {[signalsetIndex]: {signals: {[signalIndex]: {[field]: {$set: value}}}}})
    })
  }

  render() {
    return (
      <div className="parameters pane">
        <header>
          <div className="pane-title">Parameters</div>
        </header>
        <div className="pane-content">
          <div className="section-label">Reactions</div>
          <div className="section">
            <Reactions reactions={this.state.reactions}
              handleKeyPress={this.reactionsHandleKeyPress}
              handleChange={this.reactionsHandleChange}
              />
          </div>
          <div className="section-label">Signal Sets</div>
          <div className="section">
            <Signals signalsets={this.state.signalsets}
              handleKeyPress={this.signalsHandleKeyPress}
              handleChange={this.signalsHandleChange}
              />
          </div>
          <div className="section-label">Initial Molecule Counts</div>
          <div className="section">
            <Molecules molecules={this.state.molecules}
              editCount={this.moleculesEditCount}
              />
          </div>
        </div>
      </div>
    )
  }
}

export default Parameters;
