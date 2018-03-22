import React, { Component } from 'react';
import PropTypes from 'prop-types';
import Icon from '../Icon/Icon';
import Tooltip from '../Tooltip/Tooltip';

class InfoBox extends Component {
  constructor(props) {
    super(props);

    this.state = { showPopup: false };

    this.setRef = this.setRef.bind(this);
    this.togglePopup = this.togglePopup.bind(this);
  }

  setRef(what) {
    return (element) => {
      this[what] = element;
    };
  }

  get position() {
    if (!this.wrapper) return { top: '0px', left: '0px' };
    const { top, right } = this.wrapper.getBoundingClientRect();
    return { top: `${top}px`, left: `${right}px` };
  }

  togglePopup() {
    this.setState({ showPopup: !this.state.showPopup });
  }

  render() {
    const { icon, text } = this.props;
    const { showPopup } = this.state;
    return (
      <span
        ref={this.setRef('wrapper')}
        onMouseEnter={this.togglePopup}
        onMouseLeave={this.togglePopup}
      >
        <Icon icon={icon} />
        { showPopup && (
          <Tooltip fixed placement="right" style={this.position}>
            { text }
          </Tooltip>
        )}
      </span>
    );
  }
}

InfoBox.propTypes = {
  icon: PropTypes.string,
  text: PropTypes.string.isRequired,
};

InfoBox.defaultProps = {
  icon: 'info',
};

export default InfoBox;
