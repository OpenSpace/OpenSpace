import React, { Component } from 'react';
import PropTypes from 'prop-types';
import { defer } from '../../../utils/helpers';
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
    this.updateScrollIndicators = this.updateScrollIndicators.bind(this);
  }

  componentWillReceiveProps() {
    // defer this call so that the dom actually renders and the
    // properties in `this.node` used in `updateScrollIndicators` are
    // updated with the new props.
    defer(this.updateScrollIndicators, this.node);
  }

  setDomNode(node) {
    if (node) {
      this.node = node;
      this.node.addEventListener('scroll', ({ target }) => this.updateScrollIndicators(target));
      defer(this.updateScrollIndicators, this.node);
    }
  }

  /**
   * decide if the scroll indicators should be shown or not
   * @param target - the `.scroll-content` node
   */
  updateScrollIndicators(target) {
    // compare using `< 1` instead of `=== 0` because floating point precision
    const bottomHeight = target.scrollTop + target.clientHeight;
    const atBottom = Math.abs(bottomHeight - target.scrollHeight) < 1;
    const atTop = target.scrollTop === 0;
    this.setState({ atTop, atBottom });
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
        className={`scroll-content ${this.props.className} ${styles.ScrollOverlay} ${this.stateClasses}`}
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
  className: PropTypes.string,
};

ScrollOverlay.defaultProps = {
  className: '',
};

export default ScrollOverlay;
