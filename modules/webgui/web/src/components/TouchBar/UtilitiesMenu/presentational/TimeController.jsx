import React, { Component } from 'react';
import { CurrenTimeKey, DeltaTime } from '../../../../api/keys';
import DataManager, { TopicTypes } from '../../../../api/DataManager';
import styles from './../style/TimeController.scss';
import Button from '../../../common/Input/Button/Button';
import * as timeHelpers from '../../../../utils/timeHelpers';
import ScaleInput from '../../../common/Input/ScaleInput/ScaleInput';

class TimeController extends Component {
  constructor(props) {
    super(props);

    this.mounted = false;

    this.state = {
      paused: false,
      time: new Date(),
      hasTime: false,
      subscriptionIdCurrent: -1,
      subscriptionIdDelta: -1,
      deltaTime: 1,
    };

    this.handleOnTogglePause = this.handleOnTogglePause.bind(this);
    this.currentTimeCallback = this.currentTimeCallback.bind(this);
    this.deltaTimeCallback = this.deltaTimeCallback.bind(this);

    this.setDate = this.setDate.bind(this);
    this.setSimulationSpeed = this.setSimulationSpeed.bind(this);
  }


  componentDidMount() {
    this.mounted = true;
    // subscribe to data
    this.state.subscriptionIdCurrent = DataManager
      .subscribe(CurrenTimeKey, this.currentTimeCallback, TopicTypes.time);
    this.state.subscriptionIdDelta = DataManager
      .subscribe(DeltaTime, this.deltaTimeCallback, TopicTypes.time);
  }

  componentWillUnmount() {
    // TODO timetopic have no unsubscribe function, therefore this.mounted is used as a workaround.
    this.mounted = false;
    // DataManager.unsubscribe(CurrenTimeKey, this.state.subscriptionIdCurrent);
    // DataManager.unsubscribe(DeltaTime, this.state.subscriptionIdDelta);
    // this.setState({ subscriptionIdCurrent: -1, subscriptionIdDelta: -1 });
  }

  get time() {
    return this.state.time.toUTCString();
  }

  setSimulationSpeed(value) {
    this.beforeAdjust = this.beforeAdjust || this.state.deltaTime;
    const simulationSpeed = (value ** 5);
    timeHelpers.UpdateDeltaTimeNow(this.beforeAdjust * simulationSpeed);
  }

  setDate(time) {
    this.setState({ time });
    timeHelpers.setDate(time);
  }

  currentTimeCallback(message) {
    const time = new Date(timeHelpers.DateStringWithTimeZone(message.time));
    if (this.mounted) {
      this.setState({ time, hasTime: true });
    }
  }

  deltaTimeCallback({ deltaTime }) {
    if (this.mounted) {
      this.setState({ deltaTime });
    }
  }

  handleOnTogglePause() {
    timeHelpers.togglePause();
    this.setState({ paused: !this.state.paused });
  }

  render() {
    return (
      <div className={styles.TimeController}>
        <div className={styles.ButtonContainer}>
          <Button block smalltext flexgrow fixedwidth onClick={this.handleOnTogglePause}>
            {this.state.paused ? 'Play' : 'Pause'}
          </Button>
          <Button block smalltext flexgrow fixedwidth onClick={timeHelpers.now}>
            Now
          </Button>
        </div>
        <div className={styles.SimulationIncrement}>
          <div className={styles.TimeText}>
            {new Date(this.time).toLocaleString()}
          </div>
          <ScaleInput
            defaultValue={0}
            label="Simulation Speed"
            min={-10}
            max={10}
            saveValue
            onChange={this.setSimulationSpeed}
          />
        </div>
      </div>
    );
  }
}

export default TimeController;
