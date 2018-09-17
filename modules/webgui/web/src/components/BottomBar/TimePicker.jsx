import React, { Component } from 'react';
import DataManager, { TopicTypes } from '../../api/DataManager';
import LoadingString from '../common/LoadingString/LoadingString';
import Popover from '../common/Popover/Popover';
import SmallLabel from '../common/SmallLabel/SmallLabel';
import Button from '../common/Input/Button/Button';
import Calendar from '../common/Calendar/Calendar';
import Picker from './Picker';
import Time from '../common/Input/Time/Time';

import {
  TogglePauseScript,
  InterpolateTogglePauseScript,
  CurrenTimeKey,
  ValuePlaceholder,
  SetDeltaTimeScript,
  InterpolateDeltaTime
} from '../../api/keys';

import SimulationIncrement from './SimulationIncrement';
import styles from './TimePicker.scss';

/**
 * Make sure the date string contains a time zone
 * @param date
 * @param zone - the time zone in ISO 8601 format
 * @constructor
 */
const DateStringWithTimeZone = (date, zone = 'Z') =>
  (!date.includes('Z') ? `${date}${zone}` : date);

class TimePicker extends Component {
  static togglePause(e) {
    const shift = e.getModifierState("Shift");
    if (shift) {
      DataManager.runScript(TogglePauseScript);
    } else {
      DataManager.runScript(InterpolateTogglePauseScript);
    }
  }

  static realtime(e) {
    const shift = e.getModifierState("Shift");
    let script = '';
    if (shift) {
      script = SetDeltaTimeScript.replace(ValuePlaceholder, 1);
    } else {
      script = InterpolateDeltaTimeScript.replace(ValuePlaceholder, 1);
    }
    DataManager.runScript(script);
  }

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
    this.now = this.now.bind(this);
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
          <Button block smalltext onClick={TimePicker.togglePause}>
            Pause
          </Button>
          <Button block smalltext onClick={TimePicker.realtime}>
            Realtime
          </Button>
          <Button block smalltext onClick={this.now}>
            Now
          </Button>
        </div>
      </Popover>
    );
  }

  setDate(time) {
    this.setState({ time });
    // Spice, that is handling the time parsing in OpenSpace does not support
    // ISO 8601-style time zones (the Z). It does, however, always assume that UTC
    // is given.
    const fixedTimeString = time.toJSON().replace('Z', '');
    DataManager.setValue('__time', fixedTimeString);
  }

  togglePopover() {
    this.setState({ showPopover: !this.state.showPopover });
  }

  now() {
    this.setDate(new Date());
  }

  /**
   * Callback for subscription
   * @param message [object] - message object sent from Subscription
   */
  subscriptionCallback(message) {
    const time = new Date(DateStringWithTimeZone(message.time));
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
