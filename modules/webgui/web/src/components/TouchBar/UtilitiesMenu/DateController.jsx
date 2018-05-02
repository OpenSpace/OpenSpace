import React, { Component } from 'react';
import PropTypes from 'prop-types';
import SmallLabel from '../../common/SmallLabel/SmallLabel';
import Icon from '../../common/Icon/Icon';
import Popover from '../../common/Popover/Popover';
import Picker from '../../BottomBar/Picker';
import Button from '../../common/Input/Button/Button';
import * as timeHelpers from '../../../utils/timeHelpers';

import buttonStyle from './UtilitiesButtons.scss';
import styles from './DateController.scss';

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
    timeHelpers.sortDates(this.props.dateList);
    return (this.props.dateList.map(date => (
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
          {date.planet},{date.info}
        </SmallLabel>
      </Button>
    ))
    );
  }

  pickDate(e) {
    this.togglePopover();
    timeHelpers.setDate(new Date(e.target.id));
    const selectedDate = this.props.dateList.find(date => date.date === e.target.id);
    this.props.onChangeSight(selectedDate);
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

DateController.propTypes = {
  onChangeSight: PropTypes.func,
  dateList: PropTypes.arrayOf(
    PropTypes.shape({
      place: PropTypes.string,
      planet: PropTypes.string,
      location: PropTypes.object,
    }),
  ).isRequired,
};

DateController.defaultProps = {
  onChangeSight: () => {},
};

export default DateController;
