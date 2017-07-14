import React, { Component } from 'react';
import PropTypes from 'prop-types';

import styles from './Popover.scss';

class Popover extends Component {
  get arrowStyle() {
    return this.props.arrow
      .split(' ')
      .map(style => styles[style])
      .join(' ');
  }

  render() {
    return (
      <section className={`${styles.popover} ${this.arrowStyle}`}>
        { this.props.children }
      </section>
    );
  }
}

Popover.propTypes = {
  arrow: PropTypes.string,
};

Popover.defaultProps = {
  arrow: 'arrow bottom center',
};

export default Popover;
