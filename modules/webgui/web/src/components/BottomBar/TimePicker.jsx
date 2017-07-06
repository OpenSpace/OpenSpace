import React, { Component } from 'react';
import DataManager from '../../api/DataManager';
import SmallLabel from '../common/SmallLabel/SmallLabel';
import LoadingString from '../common/LoadingString/LoadingString';
import Picker from './Picker';

const TIME_KEY = 'whatever.openspace.stores.time.in';

class TimePicker extends Component {
  constructor(props) {
    super(props);

    this.state = {
      time: new Date(),
      hasTime: false,
    };

    this.subscriptionCallback = this.subscriptionCallback.bind(this);
  }

  componentDidMount() {
    // subscribe to data
    // console.log(DataManager.nextTopicId);
    DataManager.subscribe(TIME_KEY, this.subscriptionCallback);
  }

  componentWillUnmount() {
    DataManager.unsubscribe(TIME_KEY, this.subscriptionCallback);
  }

  /**
   * Callback for subscription
   * @param message [object] - message object sent from Subscription
   */
  subscriptionCallback(message) {
    const newState = {
      time: message.time,
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
      <Picker>
        <div className={Picker.Title}>
          <span className={Picker.Name}>
            <LoadingString loading={!this.state.hasTime}>
              { this.time }
            </LoadingString>
          </span>
          <SmallLabel>Date</SmallLabel>
        </div>
      </Picker>
    );
  }
}

export default TimePicker;
