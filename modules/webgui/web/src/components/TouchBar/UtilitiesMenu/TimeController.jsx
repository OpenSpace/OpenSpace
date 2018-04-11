import React, { Component } from 'react';
import {CurrenTimeKey, DeltaTime} from '../../../api/keys';
import DataManager, {TopicTypes} from "../../../api/DataManager";
import styles from './TimeController.scss';
import Button from "../../common/Input/Button/Button";
import * as timeHelpers from "../../../utils/timeHelpers";
import ScaleInput from "../../common/Input/ScaleInput/ScaleInput";

class TimeController extends Component {
  constructor(props){
    super(props);

    this.state = {
      paused: false,
      time: new Date(),
      hasTime: false,
      subscriptionId: -1,
      deltaTime: 1,
    };

    this.handleOnTogglePause = this.handleOnTogglePause.bind(this);
    this.currentTimeCallback = this.currentTimeCallback.bind(this);
    this.deltaTimeCallback = this.deltaTimeCallback.bind(this);

    this.setDate = this.setDate.bind(this);
    this.setSimulationSpeed = this.setSimulationSpeed.bind(this);

  }

  componentDidMount() {
    // subscribe to data
    this.state.subscriptionId = DataManager
      .subscribe(CurrenTimeKey, this.currentTimeCallback, TopicTypes.time);
    DataManager.subscribe(DeltaTime, this.deltaTimeCallback, TopicTypes.time);
  }

  componentWillUnmount() {
    DataManager.unsubscribe(CurrenTimeKey, subscriptionId);
    DataManager.unsubscribe(DeltaTime, this.deltaTimeCallback);
  }

  get time() {
    return this.state.time.toUTCString();
  }

  /**
   * Callback for subscription
   * @param message [object] - message object sent from Subscription
   */
  currentTimeCallback(message) {
    const time = new Date(timeHelpers.DateStringWithTimeZone(message.time));
    this.setState({ time, hasTime: true });
  }

  deltaTimeCallback({deltaTime}) {
    this.setState({deltaTime});
  }

  setDate(time) {
    this.setState({ time });
    timeHelpers.setDate(time);
  }

  handleOnTogglePause(){
    timeHelpers.togglePause();
    this.setState({paused: !this.state.paused});
  }

  setSimulationSpeed(value) {
    this.beforeAdjust = this.beforeAdjust || this.state.deltaTime;
    const simulationSpeed = (value ** 5);
    timeHelpers.UpdateDeltaTimeNow(this.beforeAdjust * simulationSpeed);
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
          { this.time }
          </div>
          <ScaleInput
            defaultValue={0}
            label="Simulation Speed"
            min={-10}
            max={10}
            saveValue={true}
            onChange={this.setSimulationSpeed}
          />
        </div>
      </div>
    );
  }
}

export default TimeController;
