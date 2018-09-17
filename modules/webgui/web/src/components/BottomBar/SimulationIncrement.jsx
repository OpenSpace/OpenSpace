import React, { Component } from 'react';
import { throttle } from 'lodash/function';
import DataManager, { TopicTypes } from '../../api/DataManager';
import { DeltaTime, ValuePlaceholder, SetDeltaTimeScript } from '../../api/keys';
import NumericInput from '../common/Input/NumericInput/NumericInput';
import Row from '../common/Row/Row';
import Select from '../common/Input/Select/Select';
import { round10 } from '../../utils/rounding';
import ScaleInput from '../common/Input/ScaleInput/ScaleInput';

const UpdateDelayMs = 1000;
// Throttle the delta time updating, so that we don't accidentally flood
// the simulation with updates.
const UpdateDeltaTimeNow = (value) => {
  const script = SetDeltaTimeScript.replace(ValuePlaceholder, value);
  DataManager.runScript(script);
};
const UpdateDeltaTime = throttle(UpdateDeltaTimeNow, UpdateDelayMs);

const Steps = {
  seconds: 'Seconds',
  minutes: 'Minutes',
  hours: 'Hours',
  days: 'Days',
  months: 'Months',
  years: 'Years',
};
const StepSizes = {
  [Steps.seconds]: 1,
  [Steps.minutes]: 60,
  [Steps.hours]: 3600,
  [Steps.days]: 86400,
  [Steps.months]: 2678400,
  [Steps.years]: 31536000,
};
const StepPrecisions = {
  [Steps.seconds]: 0,
  [Steps.minutes]: -3,
  [Steps.hours]: -4,
  [Steps.days]: -7,
  [Steps.months]: -10,
  [Steps.years]: -14,
};
const Limits = {
  [Steps.seconds]: { min: -20000, max: 20000, step: 1 },
  [Steps.minutes]: { min: -20000, max: 20000, step: 0.001 },
  [Steps.hours]: { min: -1000, max: 1000, step: 0.0001 },
  [Steps.days]: { min: -10, max: 10, step: 0.000001 },
  [Steps.months]: { min: -10, max: 10, step: 0.00000001 },
  [Steps.years]: { min: -1, max: 1, step: 0.0000000001 },
};
Object.freeze(Steps);
Object.freeze(StepSizes);
Object.freeze(StepPrecisions);
Object.freeze(Limits);

class SimulationIncrement extends Component {
  constructor(props) {
    super(props);
    this.state = {
      deltaTime: 1,
      stepSize: Steps.seconds,
      quickAdjust: 1,
    };

    this.deltaTimeUpdated = this.deltaTimeUpdated.bind(this);
    this.setDeltaTime = this.setDeltaTime.bind(this);
    this.setStepSize = this.setStepSize.bind(this);
    this.setQuickAdjust = this.setQuickAdjust.bind(this);
  }

  componentDidMount() {
    DataManager.subscribe(DeltaTime, this.deltaTimeUpdated, TopicTypes.time);
  }

  componentWillUnmount() {
    DataManager.unsubscribe(DeltaTime, this.deltaTimeUpdated);
  }

  get stepSize() {
    const { stepSize } = this.state;
    return StepSizes[stepSize];
  }

  get limits() {
    const { stepSize } = this.state;
    return Limits[stepSize];
  }

  setDeltaTime(event) {
    const { value } = event.currentTarget;
    const deltaTime = parseFloat(value) * this.stepSize;
    if (isNaN(deltaTime)) {
      return;
    }
    // optimistic ui change
    this.setState({ deltaTime });
    UpdateDeltaTime(deltaTime);
  }

  setStepSize({ value }) {
    if (!Object.values(Steps).includes(value)) return;

    this.setState({ stepSize: value });
  }

  setQuickAdjust(value) {
    if (value !== 0) {
      this.beforeAdjust = this.beforeAdjust || this.state.deltaTime;
      const quickAdjust = (value ** 5);
      UpdateDeltaTimeNow(this.beforeAdjust * quickAdjust);
    } else {
      UpdateDeltaTime.cancel();
      UpdateDeltaTimeNow(this.beforeAdjust || 1);
      this.beforeAdjust = null;
    }
  }

  deltaTimeUpdated({deltaTime}) {
    this.setState({deltaTime});
  }

  render() {
    const { deltaTime, stepSize } = this.state;
    const adjustedDelta = round10(deltaTime / this.stepSize, StepPrecisions[stepSize]);

    const options = Object.values(Steps)
      .map(step => ({ value: step, label: step }));

    return (
      <div>
        <Row>
          <NumericInput
            {...this.limits}
            onChange={this.setDeltaTime}
            placeholder={`${stepSize} / second`}
            value={adjustedDelta}
          />
          <Select
            direction="up"
            label="Display unit"
            onChange={this.setStepSize}
            options={options}
            value={stepSize}
          />
        </Row>
        <div style={{ height: '10px' }} />
        <ScaleInput
          defaultValue={0}
          label="Quick adjust"
          min={-10}
          max={10}
          onChange={this.setQuickAdjust}
        />
      </div>
    );
  }
}

export default SimulationIncrement;
