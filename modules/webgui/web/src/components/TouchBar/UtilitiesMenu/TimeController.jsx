import React, { Component } from 'react';
import { CurrenTimeKey } from '../../../api/keys';
import DataManager, {TopicTypes} from "../../../api/DataManager";
import styles from './TimeController.scss';
import Button from "../../common/Input/Button/Button";
import * as timeHelpers from "../../../utils/timeHelpers";

class TimeController extends Component {
  constructor(props){
    super(props);

    this.state = {
      paused: false,
      time: new Date(),
      hasTime: false,
      subscriptionId: -1,
    };

    this.handleOnTogglePause = this.handleOnTogglePause.bind(this);
    this.subscriptionCallback = this.subscriptionCallback.bind(this);
    this.setDate = this.setDate.bind(this);
  }

  componentDidMount() {
    // subscribe to data
    this.state.subscriptionId = DataManager
      .subscribe(CurrenTimeKey, this.subscriptionCallback, TopicTypes.time);
  }

  componentWillUnmount() {
    DataManager.unsubscribe(CurrenTimeKey, subscriptionId);
  }

  get time() {
    return this.state.time.toUTCString();
  }

  /**
   * Callback for subscription
   * @param message [object] - message object sent from Subscription
   */
  subscriptionCallback(message) {
    const time = new Date(timeHelpers.DateStringWithTimeZone(message.time));
    this.setState({ time, hasTime: true });
  }

  setDate(time) {
    this.setState({ time });
    timeHelpers.setDate(time);
  }

  handleOnTogglePause(){
    timeHelpers.togglePause();
    this.setState({paused: !this.state.paused});
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
        <div>
        <div className={styles.TimeText}>
          { this.time }
          </div>
        </div>
      </div>
    );
  }
}

export default TimeController;
