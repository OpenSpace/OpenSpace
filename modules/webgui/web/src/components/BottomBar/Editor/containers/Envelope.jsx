import React, {Component} from 'react'
import PropTypes from 'prop-types';
import Draggable from 'react-draggable'
import Point from '../presentational/Point.jsx'
import { connect } from 'react-redux';
import GraphBody from '../../../common/Graph/GraphBody'
import { toggleActiveEnvelope, toggleActivePoint, movePoint} from '../actions';

class Envelope extends Component {
  constructor(props) {
    super(props);
    this.state = {
      clickable :[true, true, true, true]
    }
    this.handleDrag = this.handleDrag.bind(this);
    this.handleClick = this.handleClick.bind(this);
  }

  pointsForEnvelopeGraph(data) {
    return data = this.props.points.map(point =>
        Object.assign({},
        {x: point.position.x + 10,
         y: 600 - point.position.y - 10,
         color: point.color}
        )
      )
  }

  handleDrag(e, ui, id) {
    this.state.clickable[id]= false;
    let position = {
        x: this.props.points[id].position.x + ui.deltaX,
        y: this.props.points[id].position.y,
      }

    if(!this.props.points[id].anchor)
      position.y = position.y + ui.deltaY;
    this.props.MovePoint(position, id, this.props.id);
  }

  handleClick(pointId) {
    if (this.state.clickable[pointId] === false) {
      this.state.clickable[pointId]= true;
    }
    else {
      const {active, id} = this.props;
      if (active === true) {
        this.props.TogglePoint(id, pointId);
      }
      else if(this.hasActiveChild()) {
        this.props.TogglePoint(id, pointId);
      }
      else {
        this.props.ToggleEnvelope(id);
      }
    }
  }

  hasActiveChild() {
    var hasActiveChild = false;
    this.props.points.forEach(function(point) {
      if (point.active)
        hasActiveChild = true;
    })
    return hasActiveChild;
  }

  render() {
    const { points, height, width, active } = this.props;
    return (
      <div className={styles.Envelope}>
        <svg className={styles.Line} height={height} width={width + 10}>
          <GraphBody
           UseLinearGradient={true}
           points={this.pointsForEnvelopeGraph(points)}
           x={0}
           y={600}
           width={800}
           fillOpacity={"0"}
           strokeWidth={2}
          />
        </svg>
       {points.map((point) =>
        <Point
        key={point.id}
          handleClick={() => this.handleClick(point.id)}
          handleDrag={(e, ui) => this.handleDrag(e, ui, point.id)}
          height={height}
          width={width}
          {...point}
          active={(point.active || active) ? true : false}

        />
        )}
      </div>
    );
  }
}
Envelope.propTypes = {
  points: PropTypes.arrayOf(
    PropTypes.shape({
      id: PropTypes.number.isRequired,
      position: PropTypes.shape({
              x: PropTypes.number.isRequired,
              y: PropTypes.number.isRequired,
            }).isRequired,
      anchor: PropTypes.bool.isRequired,
      color: PropTypes.string.isRequired,
    }).isRequired,
  ).isRequired,
  height: PropTypes.number.isRequired,
  width: PropTypes.number.isRequired,
  id: PropTypes.number.isRequired,
  active: PropTypes.bool.isRequired,
}

const mapDispatchToProps = (dispatch) => {
  return {
    ToggleEnvelope: (id) => {
      dispatch(toggleActiveEnvelope(id));
    },
    TogglePoint: (envelopeId, pointId) => {
      dispatch(toggleActivePoint(envelopeId, pointId));
    },
    MovePoint: (position, id, envelopeId) => {
      dispatch(movePoint(id, envelopeId, position));
    },
  }
}

Envelope = connect(
  null,
  mapDispatchToProps,
  )(Envelope)

export default Envelope;
