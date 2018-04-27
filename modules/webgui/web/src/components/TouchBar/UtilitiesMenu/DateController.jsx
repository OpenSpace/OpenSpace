import React, { Component } from 'react';
import SmallLabel from '../../common/SmallLabel/SmallLabel';
import Icon from '../../common/Icon/Icon';
import Popover from '../../common/Popover/Popover';
import Picker from '../../BottomBar/Picker';
import Button from '../../common/Input/Button/Button';
import * as timeHelpers from '../../../utils/timeHelpers';

import buttonStyle from './UtilitiesButtons.scss';
import styles from './DateController.scss';

// TODO Remove and replace with input from API
const dateList = [
  { date: '2015-07-14', info: 'Cool Space event' },
  { date: '2019-07-14', info: 'Awesome Space event' },
  { date: '2018-07-04', info: 'Great Space event' },
  { date: '2014-08-04', info: 'Whoho Space event' },
];

class DateController extends Component {
  constructor(props) {
    super(props);

    this.state = {
      showPopover: false,
    };

    this.togglePopover = this.togglePopover.bind(this);
    this.pickDate = this.pickDate.bind(this);
  }

  get popover() {
    return (
      <Popover
        className={Picker.Popover}
        title="Select event"
        closeCallback={this.togglePopover}
      >
        <div>
          {this.dateButtons}
        </div>
      </Popover>
    );
  }

  get dateButtons() {
    timeHelpers.sortDates(dateList);
    return (dateList.map(date => (
      <Button
        className={styles.dateButton}
        id={date.date}
        key={date.date}
        smalltext
        block
        onClick={this.pickDate}
      >
        <span className={styles.date} id={date.date}>
          {new Date(date.date).toLocaleDateString()}
        </span>
        <SmallLabel className={styles.label} id={date.date}>
          {date.info}
        </SmallLabel>
      </Button>
    ))
    );
  }

  pickDate(e) {
    this.togglePopover();
    timeHelpers.setDate(new Date(e.target.id));
  }

  togglePopover() {
    this.setState({ showPopover: !this.state.showPopover });
  }

  render() {
    return (
      <div className={Picker.Wrapper}>
        <Picker onClick={this.togglePopover} className={`${styles.dateController} ${this.state.showPopover ? Picker.Active : ''}`}>
          <Icon icon="date_range" className={buttonStyle.Icon} />
          <SmallLabel>Select event</SmallLabel>
        </Picker>
        { this.state.showPopover && this.popover }
      </div>
    );
  }
}


export default DateController;
