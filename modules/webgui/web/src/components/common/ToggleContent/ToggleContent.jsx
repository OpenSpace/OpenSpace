import React, { Component } from 'react';
import PropTypes from 'prop-types';
import ToggleHeader from './ToggleHeader';

import styles from './ToggleContent.scss';

class ToggleContent extends Component {
  constructor(props) {
    super(props);

    this.state = {
      toggled: props.show,
    };

    this.toggleExpanded = this.toggleExpanded.bind(this);
  }

  toggleExpanded() {
    this.setState({ toggled: !this.state.toggled });
  }

  render() {
    const { children, title } = this.props;
    const { toggled } = this.state;

    return (
      <div className={styles.toggleContent}>
        <ToggleHeader title={title} onClick={this.toggleExpanded} toggled={toggled} />
        <div className={styles.content}>
          { toggled && children }
        </div>
      </div>
    );
  }
}

ToggleContent.propTypes = {
  children: PropTypes.node,
  show: PropTypes.bool,
  title: PropTypes.string.isRequired,
};

ToggleContent.defaultProps = {
  children: '',
  show: false,
};

export default ToggleContent;
