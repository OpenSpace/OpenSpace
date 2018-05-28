import React, { Component } from 'react';
import PropTypes from 'prop-types';
import Icon from '../../../common/Icon/Icon';
import SmallLabel from '../../../common/SmallLabel/SmallLabel';
import Picker from '../../../BottomBar/Picker';
import Popover from '../../../common/Popover/Popover';
import buttonStyle from './../style/UtilitiesButtons.scss';

class InfoButton extends Component {
  constructor(props) {
    super(props);

    this.state = {
      showPopover: false,
    };

    this.togglePopover = this.togglePopover.bind(this);
  }

  get popover() {
    return (
      <Popover
        className={Picker.Popover}
        title={this.props.storyTitle}
        closeCallback={this.togglePopover}
      >
        <p>
          {this.props.storyInfo}
        </p>
      </Popover>
    );
  }

  togglePopover() {
    this.setState({ showPopover: !this.state.showPopover });
  }

  render() {
    return (
      <div className={Picker.Wrapper}>
        <Picker
          onClick={this.togglePopover}
          className={`${buttonStyle.UtilitiesButton}
          ${this.state.showPopover && buttonStyle.active} ${this.state.showPopover && Picker.Active}`}
        >
          <Icon icon="info_outline" className={buttonStyle.Icon} />
          <SmallLabel>Info</SmallLabel>
        </Picker>
        { this.state.showPopover && this.popover }
      </div>
    );
  }
}

InfoButton.propTypes = {
  storyTitle: PropTypes.string.isRequired,
  storyInfo: PropTypes.string.isRequired,
};

export default InfoButton;
