import React, { Component } from 'react';
import PropTypes from 'prop-types';
import ToggleHeader from './ToggleHeader';
import Icon from '../Icon/Icon';

import styles from './ToggleContent.scss';

class ToggleContent extends Component {
  constructor(props) {
    super(props);

    this.state = {
      toggled: props.show,
      hovered: false
    };

    this.toggleExpanded = this.toggleExpanded.bind(this);
    this.mouseEntered = this.mouseEntered.bind(this);
    this.mouseLeft = this.mouseLeft.bind(this);
  }

  toggleExpanded() {
    this.setState({ toggled: !this.state.toggled });
  }

  mouseEntered() {
    this.setState({ hovered: true });
  }

  mouseLeft() {
    this.setState({ hovered: false });
  }

  render() {
    const { children, headerChildren, title, shyHeaderChildren } = this.props;
    const { toggled } = this.state;

    return children.length !== 0 ? (
      <div className={styles.toggleContent}
           onMouseEnter={this.mouseEntered}
           onMouseLeave={this.mouseLeft}>
        <ToggleHeader
          title={title}
          onClick={this.toggleExpanded}

          children={(!shyHeaderChildren || this.state.hovered) && headerChildren}
          toggled={toggled}
        />
        <div className={styles.content}>
          { toggled && children }
        </div>
      </div>
    ) : null;
  }
}

ToggleContent.propTypes = {
  children: PropTypes.node,
  headerChildren: PropTypes.node,
  shyHeaderChildren: PropTypes.bool,
  show: PropTypes.bool,
  title: PropTypes.string.isRequired,
};

ToggleContent.defaultProps = {
  children: '',
  show: false,
  shy: false,
};

export default ToggleContent;
