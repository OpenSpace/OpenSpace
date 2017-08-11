import React, { Component } from 'react';
import DataManager from '../../api/DataManager';
import LoadingString from '../common/LoadingString/LoadingString';
import Popover from '../common/Popover/Popover';
import SmallLabel from '../common/SmallLabel/SmallLabel';
import Button from '../common/Input/Button';
import Calendar from '../common/Calendar/Calendar';
import Picker from './Picker';
import Time from '../common/Input/Time';

const TIME_KEY = 'special:currentTime';

/**
 * Make sure the date string contains a time zone
 * @param date
 * @param zone - the time zone in ISO 8601 format
 * @constructor
 */
const DateStringWithTimeZone = (date, zone = 'Z') =>
  (!date.includes('Z') ? `${date}${zone}` : date);

class TimePicker extends Component {
  constructor(props) {
    super(props);

    this.state = {
      time: new Date(),
      hasTime: false,
      showPopover: false,
    };

    this.subscriptionCallback = this.subscriptionCallback.bind(this);
    this.togglePopover = this.togglePopover.bind(this);
    this.setDate = this.setDate.bind(this);
  }

  componentDidMount() {
    // subscribe to data
    DataManager.subscribe(TIME_KEY, this.subscriptionCallback);
  }

  componentWillUnmount() {
    DataManager.unsubscribe(TIME_KEY, this.subscriptionCallback);
  }

  togglePopover() {
    this.setState({ showPopover: !this.state.showPopover });
  }

  /**
   * Callback for subscription
   * @param message [object] - message object sent from Subscription
   */
  subscriptionCallback(message) {
    const time = new Date(DateStringWithTimeZone(message.time));
    this.setState({ time, hasTime: true });
  }

  setDate(time) {
    this.setState({ time });
    // Spice, that is handling the time parsing in OpenSpace does not suppport
    // ISO 8601-style time zones (the Z). It does, however, always assume that UTC
    // is given.
    const fixedTimeString = time.toJSON().replace('Z', '');
    DataManager.setValue('__time', fixedTimeString);
  }

  get time() {
    return this.state.time.toUTCString();
  }

  get popover() {
    const { time } = this.state;
    return (
      <Popover className={Picker.Popover} title="Select date" closeCallback={this.togglePopover}>
        <Calendar selected={time} activeMonth={time} onChange={this.setDate} todayButton />
        <hr className={Popover.styles.delimiter} />
        <div className={Popover.styles.title}>Select time</div>
        <div className={Popover.styles.content}>
          <Time time={time} onChange={this.setDate} />
        </div>
        <hr className={Popover.styles.delimiter} />
        <p>
          <Button block disabled onClick={this.togglePause}>
            Pause simulation
          </Button>
        </p>
      </Popover>
    );
  }

  render() {
    const { showPopover } = this.state;
    return (
      <div className={Picker.Wrapper}>
        <Picker onClick={this.togglePopover} className={(showPopover ? Picker.Active : '')}>
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
