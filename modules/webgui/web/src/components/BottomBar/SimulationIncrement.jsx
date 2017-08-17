import React, { Component } from 'react';
import { throttle } from 'lodash/function';
import DataManager, { TopicTypes } from '../../api/DataManager';
import { DeltaTime, ValuePlaceholder, SetDeltaTimeScript } from '../../api/keys';
import NumericInput from '../common/Input/NumericInput';
import Row from '../common/Row/Row';

const UpdateDelayMs = 1000;
// Throttle the delta time updating, so that we don't accidentally flood
// the simulation with updates.
const UpdateDeltaTime = throttle((value) => {
  const script = SetDeltaTimeScript.replace(ValuePlaceholder, value);
  DataManager.runScript(script);
}, UpdateDelayMs);

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
    const { value } = event.currentTarget;
    // optimistic ui change
    this.setState({ deltaTime: value });
    UpdateDeltaTime(value);
  }

  deltaTimeUpdated({ deltaTime }) {
    this.setState({ deltaTime });
  }

  render() {
    return (
      <Row>
        {/* TODO: Add debouncing support to inputs */}
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
