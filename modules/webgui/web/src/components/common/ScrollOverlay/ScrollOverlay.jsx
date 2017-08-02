import React, { Component } from 'react';
import PropTypes from 'prop-types';
import styles from './ScrollOverlay.scss';

/**
 * Limit the vertical space an element, or a set of element, may take.
 * Adds a scroll bar and indicators for more content.
 *
 * Exposes the global CSS class `scroll-content`, so that it may be styled.
 */
class ScrollOverlay extends Component {
  static get elementId() {
    // eslint-disable-next-line no-plusplus
    return `scroll-overlay-${ScrollOverlay.elementCount++}`;
  }

  constructor(props) {
    super(props);

    this.state = {
      id: ScrollOverlay.elementId,
      atTop: true,
      atBottom: false,
    };
    this.setDomNode = this.setDomNode.bind(this);
    this.onScroll = this.onScroll.bind(this);
  }

  onScroll(event) {
    const { target } = event;
    const atTop = target.scrollTop === 0;
    const atBottom = target.scrollTop + target.clientHeight === target.scrollHeight;
    this.setState({ atTop, atBottom });
  }

  setDomNode(node) {
    if (node) {
      this.node = node;
      this.node.addEventListener('scroll', this.onScroll);
    }
  }

  get hasScrollBar() {
    if (!this.node || !this.node.scrollHeight) {
      return false;
    }

    return this.node.clientHeight < this.node.scrollHeight;
  }

  get stateClasses() {
    if (!this.hasScrollBar) return false;

    let classes = '';
    if (!this.state.atBottom) {
      classes += `${styles.notAtBottom} `;
    }
    if (!this.state.atTop) {
      classes += styles.notAtTop;
    }

    return classes;
  }

  render() {
    return (
      <div
        className={`scroll-content ${styles.ScrollOverlay} ${this.stateClasses}`}
        id={this.state.id}
        ref={this.setDomNode}
      >
        { this.props.children }
      </div>
    );
  }
}

ScrollOverlay.elementCount = 0;

ScrollOverlay.propTypes = {
  children: PropTypes.node.isRequired,
};

export default ScrollOverlay;
