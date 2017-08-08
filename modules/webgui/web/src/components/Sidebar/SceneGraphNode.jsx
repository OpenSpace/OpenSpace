import React, { Component } from 'react';
import PropTypes from 'prop-types';
import Icon from '../common/Icon/Icon';

import styles from './SceneGraphNode.scss';

class SceneGraphNode extends Component {
  constructor(props) {
    super(props);
    this.state = { expanded: props.expanded };

    this.toggleExpand = this.toggleExpand.bind(this);
  }

  state: {
    expanded: boolean,
  };
  props: {
    name: string,
    properties: Array<any>,
    renderable: Array<any>,
  };

  toggleExpand() {
    this.setState({ expanded: !this.state.expanded });
  }

  render() {
    const { name, properties, renderable } = this.props;
    const { expanded } = this.state;

    return (
      <article className={styles.sceneGraphNode}>
        <header onClick={this.toggleExpand} role="button" tabIndex={0}>
          <div className={styles.title}>
            { name }
          </div>
          <Icon icon={expanded ? 'expand_more' : 'chevron_left'} className={styles.expandIcon} />
        </header>

        { expanded && (
          <span>
            { JSON.stringify(properties) }
            { JSON.stringify(renderable) }
          </span>
        )}
      </article>
    );
  }
}

SceneGraphNode.propTypes = {
  expanded: PropTypes.boolean,
  properties: PropTypes.arrayOf(PropTypes.object),
  renderable: PropTypes.arrayOf(PropTypes.object),
};

SceneGraphNode.defaultProps = {
  expanded: false,
  properties: [],
  renderable: [],
};

export default SceneGraphNode;
