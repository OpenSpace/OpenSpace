import React, { Component } from 'react';
import SmallLabel from '../common/SmallLabel/SmallLabel';
import Picker from './Picker';

class TimePicker extends Component {
  constructor(props) {
    super(props);

    this.state = {
      time: new Date(),
    };
  }

  get time() {
    return this.state.time.toUTCString();
  }

  render() {
    return (
      <Picker>
        <div className={Picker.Title}>
          <span className={Picker.Name}>{ this.time }</span>
          <SmallLabel>Date</SmallLabel>
        </div>
      </Picker>
    );
  }
}

export default TimePicker;
