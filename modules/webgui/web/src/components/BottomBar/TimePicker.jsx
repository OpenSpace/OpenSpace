import React, { Component } from 'react';
import DataManager from '../../api/DataManager';
import LoadingString from '../common/LoadingString/LoadingString';
import Popover from '../common/Popover/Popover';
import SmallLabel from '../common/SmallLabel/SmallLabel';
import Calendar from '../common/Calendar/Calendar';
import Picker from './Picker';

const TIME_KEY = 'special:currentTime';

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
    const newState = {
      time: new Date(message.time),
    };
    if (!this.state.hasTime) {
      newState.hasTime = true;
    }
    this.setState(previous => Object.assign({}, previous, newState));
  }

  get time() {
    return this.state.time.toUTCString();
  }

  render() {
    return (
      <div className={Picker.Wrapper}>
        <Picker onClick={this.togglePopover}>
          <div className={Picker.Title}>
            <span className={Picker.Name}>
              <LoadingString loading={!this.state.hasTime}>
                { this.time }
              </LoadingString>
            </span>
            <SmallLabel>Date</SmallLabel>
          </div>
        </Picker>
        { this.state.showPopover && (
          <Popover className={Picker.Popover} title="Select time" closeCallback={this.togglePopover}>
            <Calendar selected={this.state.time} />
          </Popover>
        )}
      </div>
    );
  }
}

export default TimePicker;
