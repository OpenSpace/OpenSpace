import React, { Component } from 'react';
import PropTypes from 'prop-types';
import Picker from '../../BottomBar/Picker';
import Icon from '../../common/Icon/Icon';
import SmallLabel from '../../common/SmallLabel/SmallLabel';

import buttonStyle from './UtilitiesButtons.scss';
import styles from './SightsController.scss';
import Popover from '../../common/Popover/Popover';
import Button from '../../common/Input/Button/Button';


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
    return (this.props.sightsList.map(sight => (
      <Button
        key={sight.place}
        smalltext
        block
        onClick={this.selectSight}
        id={sight.place}
      >
        <SmallLabel id={sight.place} style={{ float: 'left' }}>
          {sight.planet},{sight.place}
        </SmallLabel>
      </Button>
    )));
  }

  selectSight(e) {
    this.togglePopover();
    const selectedSight = this.props.sightsList.find(sight => sight.place === e.target.id);
    this.props.onChangeSight(selectedSight);
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

SightsController.propTypes = {
  onChangeSight: PropTypes.func,
  sightsList: PropTypes.arrayOf(
    PropTypes.shape({
      place: PropTypes.string,
      planet: PropTypes.string,
      location: PropTypes.object,
    }),
  ).isRequired,
};

SightsController.defaultProps = {
  onChangeSight: () => {},
};


export default SightsController;
