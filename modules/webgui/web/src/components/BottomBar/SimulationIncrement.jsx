import React, { Component } from 'react';
import DataManager, { TopicTypes } from '../../api/DataManager';
import { DeltaTime, ValuePlaceholder, SetDeltaTimeScript } from '../../api/keys';
import NumericInput from '../common/Input/NumericInput';
import Row from '../common/Row/Row';

class SimulationIncrement extends Component {
  constructor(props) {
    super(props);
    this.state = { deltaTime: 1 };

    this.deltaTimeUpdated = this.deltaTimeUpdated.bind(this);
    this.setDeltaTime = this.setDeltaTime.bind(this);
  }

  componentDidMount() {
    DataManager.subscribe(DeltaTime, this.deltaTimeUpdated, TopicTypes.time);
  }

  componentWillUnmount() {
    DataManager.unsubscribe(DeltaTime, this.deltaTimeUpdated);
  }

  setDeltaTime(event) {
    // TODO: do this deffered!!

    const { value } = event.currentTarget;
    // optimistic ui change
    this.setState({ deltaTime: value });
    const script = SetDeltaTimeScript.replace(ValuePlaceholder, value);
    DataManager.runScript(script);
  }

  deltaTimeUpdated({ deltaTime }) {
    this.setState({ deltaTime });
  }

  render() {
    return (
      <Row>
        {/* TODO: Add deffering support to inputs */}
        <NumericInput
          value={this.state.deltaTime}
          placeholder="Seconds per step"
          min={-100000}
          max={100000}
          onChange={this.setDeltaTime}
        />
      </Row>
    );
  }
}

export default SimulationIncrement;
