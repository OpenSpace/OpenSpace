import React, { Component } from 'react';
import Picker from '../../BottomBar/Picker';
import Icon from '../../common/Icon/Icon';
import SmallLabel from '../../common/SmallLabel/SmallLabel';

import buttonStyle from './UtilitiesButtons.scss';
import styles from './SightsController.scss';
import Popover from '../../common/Popover/Popover';
import Button from '../../common/Input/Button/Button';

// TODO Remove and replace with input from API
const sightsList = ['Stockholm', 'Salt Lake City', 'New York'];

class SightsController extends Component {
  constructor(props) {
    super(props);

    this.state = {
      showPopover: false,
    };

    this.togglePopover = this.togglePopover.bind(this);
    this.selectSight = this.selectSight.bind(this);
  }

  get popover() {
    return (
      <Popover
        className={`${Picker.Popover} ${styles.popover}`}
        title="Select sight"
        closeCallback={this.togglePopover}
      >
        {this.sightsButtons}
      </Popover>
    );
  }

  get sightsButtons() {
    return (sightsList.map(sight => (
      <Button
        key={sight}
        smalltext
        block
        onClick={this.selectSight}
        id={sight}
      >
        <SmallLabel id={sight} style={{ float: 'left' }}>
          {sight}
        </SmallLabel>
      </Button>
    )));
  }

  selectSight(e) {
    this.togglePopover();
    // console.log('Select Sight', e.target.id);
  }

  togglePopover() {
    this.setState({ showPopover: !this.state.showPopover });
  }

  render() {
    return (
      <div className={Picker.Wrapper} >
        <Picker onClick={this.togglePopover} className={`${styles.sightsController} ${this.state.showPopover ? Picker.Active : ''}`}>
          <Icon icon="place" className={buttonStyle.Icon} />
          <SmallLabel>Select sight</SmallLabel>
        </Picker>
        { this.state.showPopover && this.popover }
      </div>
    );
  }
}

export default SightsController;
