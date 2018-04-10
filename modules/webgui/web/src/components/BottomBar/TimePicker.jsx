import React, { Component } from 'react';
import DataManager, { TopicTypes } from '../../api/DataManager';
import LoadingString from '../common/LoadingString/LoadingString';
import Popover from '../common/Popover/Popover';
import SmallLabel from '../common/SmallLabel/SmallLabel';
import Button from '../common/Input/Button/Button';
import Calendar from '../common/Calendar/Calendar';
import Picker from './Picker';
import Time from '../common/Input/Time/Time';
import { CurrenTimeKey } from '../../api/keys';
import SimulationIncrement from './SimulationIncrement';
import styles from './TimePicker.scss';
import * as timeHelpers from '../../utils/timeHelpers';

class TimePicker extends Component {
  constructor(props) {
    super(props);

    this.state = {
      time: new Date(),
      hasTime: false,
      showPopover: false,
      subscriptionId: -1,
    };

    this.subscriptionCallback = this.subscriptionCallback.bind(this);
    this.togglePopover = this.togglePopover.bind(this);
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

  get popover() {
    const { time } = this.state;
    return (
      <Popover
        className={Picker.Popover}
        title="Select date"
        closeCallback={this.togglePopover}
        detachable
      >
        <Calendar selected={time} activeMonth={time} onChange={this.setDate} todayButton />
        <hr className={Popover.styles.delimiter} />
        <div className={Popover.styles.title}>Select local time</div>
        <div className={Popover.styles.content}>
          <Time time={time} onChange={this.setDate} />
        </div>
        <hr className={Popover.styles.delimiter} />

        <div className={Popover.styles.title}>Simulation speed</div>
        <div className={Popover.styles.content}>
          <SimulationIncrement />
        </div>
        <hr className={Popover.styles.delimiter} />

        <div className={`${Popover.styles.row} ${Popover.styles.content}`}>
          <Button block smalltext onClick={timeHelpers.togglePause}>
            Pause
          </Button>
          <Button block smalltext onClick={timeHelpers.realtime}>
            Realtime
          </Button>
          <Button block smalltext onClick={timeHelpers.now}>
            Now
          </Button>
        </div>
      </Popover>
    );
  }

  setDate(time) {
    this.setState({ time });
    timeHelpers.setDate(time);
  }

  togglePopover() {
    this.setState({ showPopover: !this.state.showPopover });
  }

  /**
   * Callback for subscription
   * @param message [object] - message object sent from Subscription
   */
  subscriptionCallback(message) {
    const time = new Date(timeHelpers.DateStringWithTimeZone(message.time));
    this.setState({ time, hasTime: true });
  }

  render() {
    const { showPopover } = this.state;
    return (
      <div className={Picker.Wrapper}>
        <Picker onClick={this.togglePopover} className={`${styles.timePicker} ${showPopover ? Picker.Active : ''}`}>
          <div className={Picker.Title}>
            <span className={Picker.Name}>
              <LoadingString loading={!this.state.hasTime}>
                { this.time }
              </LoadingString>
            </span>
            <SmallLabel>Date</SmallLabel>
          </div>
        </Picker>

        { showPopover && this.popover }
      </div>
    );
  }
}

export default TimePicker;
